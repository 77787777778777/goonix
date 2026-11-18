{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- The unsandboxed build interpreter.  Its process execution is deliberately
-- kept separate from the pure scheduling model in Goonix.Build.Scheduler.
module Goonix.Build.Engine
  ( BuildMode (..)
  , buildThings
  , buildThingsWithTrace
  , buildThingsWithConfig
  , BuildTrace (..)
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally, throwIO)
import Control.Monad (forM_, replicateM_, unless, void, when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Goonix.Store.Base32 (decodeBase16, decodeBase32, encodeBase32)
import Goonix.Store.Derivation
import Goonix.Store.Files
import Goonix.Store.Hash
import Goonix.Store.Nar (dumpNar, hashNar)
import Goonix.Store.ReadOnly
import Goonix.Store.References (scanReferences)
import Goonix.Store.Settings
import Goonix.Store.Config (ServerConfig)
import Goonix.Store.Substitute (substitutableInfos, substitutePath)
import Goonix.Store.Writing
import System.Posix.ByteString (RawFilePath)
import qualified System.Posix.Directory.ByteString as Dir
import qualified System.Posix.Files.ByteString as Files
import System.Exit (ExitCode (ExitSuccess))
import System.IO (Handle, IOMode (WriteMode), hClose, openBinaryFile)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc, waitForProcess)
import System.Posix.Process (getProcessID)
import System.Posix.IO.ByteString (createPipe, fdToHandle)
import System.FilePath (takeDirectory)
import System.Posix.Types (Fd)
import System.Environment (getEnvironment)
import System.Posix.Env.ByteString (getEnv)

data BuildMode = Normal | Repair | Check deriving (Eq, Show)
data BuildTrace = BuildTrace
  { traceStarted :: ByteString -> ByteString -> ByteString -> Int -> IO ()
  , traceSucceeded :: ByteString -> IO ()
  , traceFailed :: ByteString -> String -> IO ()
  , traceOutput :: ByteString -> IO ()
  }

buildThingsWithTrace :: Fd -> Store -> Settings -> BuildTrace -> [ByteString] -> BuildMode -> IO ()
buildThingsWithTrace rootFd store settings trace things mode =
  buildThingsInternal Nothing rootFd store settings trace things mode

buildThingsWithConfig :: ServerConfig -> Fd -> Store -> Settings -> BuildTrace -> [ByteString] -> BuildMode -> IO ()
buildThingsWithConfig config = buildThingsInternal (Just config)

buildThingsInternal :: Maybe ServerConfig -> Fd -> Store -> Settings -> BuildTrace -> [ByteString] -> BuildMode -> IO ()
buildThingsInternal config rootFd store settings trace things mode = do
  unless (not (settingBool "build-use-sandbox" True settings)) $
    throwIO (storeError "sandboxed builds are not implemented")
  mapM_ buildOne things
  where
    buildOne thing
      | B.isSuffixOf ".drv" thing = buildDerivation config rootFd store settings trace mode thing
      | B.isInfixOf "!" thing = do
          let (drv, _) = B.break (== 33) thing
          buildOne drv
      | otherwise = case config of
          Just serverConfig -> void (trySubstitute serverConfig rootFd store settings trace thing)
          Nothing -> pure ()

buildThings :: Fd -> Store -> Settings -> [ByteString] -> BuildMode -> IO ()
buildThings rootFd store settings things mode =
  buildThingsWithTrace rootFd store settings (BuildTrace (\_ _ _ _ -> pure ()) (\_ -> pure ()) (\_ _ -> pure ()) (\_ -> pure ())) things mode

buildDerivation :: Maybe ServerConfig -> Fd -> Store -> Settings -> BuildTrace -> BuildMode -> ByteString -> IO ()
buildDerivation config rootFd store settings trace mode drvPath = do
  appendTempRoot rootFd drvPath
  valid <- isValidPath store drvPath
  unless valid $ throwIO (storeError ("derivation `" ++ C.unpack drvPath ++ "' is not valid"))
  bytes <- B.readFile (C.unpack drvPath)
  drv <- either (throwIO . storeError) pure (parseDerivation bytes)
  forM_ (Map.keys (inputDrvs drv)) $ \input -> buildDerivation config rootFd store settings trace mode input
  forM_ (Set.toList (inputSrcs drv)) $ \input -> do
    present <- isValidPath store input
    unless present $ throwIO (storeError ("input `" ++ C.unpack input ++ "' is not valid"))
  ready <- allOutputsValid store drv
  substituted <- case config of
    Just serverConfig | mode /= Check && not ready && settingBool "build-use-substitutes" False settings ->
      or <$> mapM (trySubstitute serverConfig rootFd store settings trace . outputPath) (Map.elems (outputs drv))
    _ -> pure False
  when (mode == Repair || mode == Check || not (ready || substituted)) $ do
    runBuilder rootFd store settings trace mode drvPath drv
    when (mode == Normal) $ replicateM_ (max 0 (settingInt "build-repeat" 0 settings)) $
      runBuilder rootFd store settings trace Check drvPath drv
  pure ()

trySubstitute :: ServerConfig -> Fd -> Store -> Settings -> BuildTrace -> ByteString -> IO Bool
trySubstitute config rootFd store settings trace path = do
  valid <- isValidPath store path
  if valid then pure True else do
    infos <- substitutableInfos config settings [path]
    case infos of
      [] -> pure False
      ((_, deriver, references, _, _):_) -> do
        mapM_ (appendTempRoot rootFd) (path:references)
        present <- mapM (isValidPath store) references
        unless (and present) (throwIO (storeError ("substitute references of `" ++ C.unpack path ++ "' are not valid")))
        withPathLock path $ do
          validNow <- isValidPath store path
          if validNow then pure True else do
            deleteTree path
            traceOutput trace ("@ substituter-started " <> path <> " substitute\n")
            result <- substitutePath config settings path path
            case result of
              Left message -> do
                traceOutput trace ("@ substituter-failed " <> path <> " " <> message <> "\n")
                if settingBool "build-fallback" False settings
                  then pure False
                  else throwIO (storeError ("fetching path `" ++ C.unpack path ++ "' failed: " ++ C.unpack message))
              Right (printedHash, size) -> do
                digest <- case C.break (== ':') printedHash of
                  ("sha256", encoded) -> either (throwIO . storeError) pure (decodeBase32 (B.drop 1 encoded))
                  _ -> throwIO (storeError "unsupported substitute hash")
                exists <- Files.fileExist path
                unless exists $ throwIO (storeError ("substitute did not produce path `" ++ C.unpack path ++ "'"))
                registerPath store path (Hash SHA256 digest) size references deriver
                traceOutput trace ("@ substituter-succeeded " <> path <> "\n")
                pure True

allOutputsValid :: Store -> Derivation -> IO Bool
allOutputsValid store drv = and <$> mapM (isValidPath store . outputPath) (Map.elems (outputs drv))

runBuilder :: Fd -> Store -> Settings -> BuildTrace -> BuildMode -> ByteString -> Derivation -> IO ()
runBuilder rootFd store settings trace mode drvPath drv = do
  let outPaths = map outputPath (Map.elems (outputs drv))
      redirected = if mode == Check then map (redirectOutput store drvPath) outPaths else outPaths
      replacements = zip outPaths redirected
      actualDrv = if mode == Check then replaceDerivationPaths replacements drv else drv
  mapM_ (appendTempRoot rootFd) outPaths
  mapM_ deleteTree redirected
  work <- makeBuildDirectory store
  env <- builderEnvironment store settings work actualDrv
  logPath <- buildLogPath store drvPath
  mkdirParents (C.pack (takeDirectory (C.unpack logPath)))
  logHandle <- openBinaryFile (C.unpack logPath) WriteMode
  (readFd, writeFd) <- createPipe
  writeHandle <- fdToHandle writeFd
  readHandle <- fdToHandle readFd
  guixProgram <- maybe "guix" C.unpack <$> getEnv "GUIX"
  let command = if "builtin:" `B.isPrefixOf` builder drv
        then proc guixProgram ["perform-download", C.unpack drvPath, C.unpack (outputPath (outputs drv Map.! "out"))]
        else proc (C.unpack (builder drv)) (map C.unpack (args drv))
  (_, _, _, process) <- createProcess command
    { cwd = Just (C.unpack work), env = Just env, std_out = UseHandle writeHandle, std_err = UseHandle writeHandle }
  childPid <- maybe 0 fromIntegral <$> getPid process
  traceStarted trace drvPath (platform drv) logPath childPid
  hClose writeHandle
  drained <- newEmptyMVar
  _ <- forkIO $ copyLog childPid (settingInt "multiplexed-build-output" 0 settings /= 0) readHandle logHandle trace `finally` putMVar drained ()
  status <- waitForProcess process
  takeMVar drained
  hClose readHandle
  hClose logHandle
  case status of
    ExitSuccess -> do
      if mode == Check
        then checkBuiltOutputs store drvPath replacements (keepFailed settings)
        else registerBuiltOutputs store drvPath drv
      removeBuildDirectory work
      traceSucceeded trace drvPath
    _ -> buildFailed work
  where
    buildFailed work = do
      traceFailed trace drvPath "builder failed"
      if keepFailed settings
        then pure ()
        else removeBuildDirectory work
      throwIO (storeError ("builder for `" ++ C.unpack drvPath ++ "' failed"))

redirectOutput :: Store -> ByteString -> RawFilePath -> RawFilePath
redirectOutput store drvPath path =
  let prefix = storeDirectory store <> "/"
      name = B.drop (B.length prefix + 32) path
      digest = encodeBase32 (sha256 ("rewrite:" <> drvPath <> ":" <> path))
  in prefix <> B.take 32 digest <> name

replaceDerivationPaths :: [(ByteString, ByteString)] -> Derivation -> Derivation
replaceDerivationPaths replacements drv = drv
  { outputs = Map.map (\output -> output { outputPath = replace (outputPath output) }) (outputs drv)
  , args = map replace (args drv)
  , environment = Map.map replace (environment drv)
  }
  where
    replace bytes = foldl (\current (old, new) -> replaceBytes old new current) bytes replacements

replaceBytes :: ByteString -> ByteString -> ByteString -> ByteString
replaceBytes old new bytes = B.intercalate new (splitBytes old bytes)

splitBytes :: ByteString -> ByteString -> [ByteString]
splitBytes separator input = case B.breakSubstring separator input of
  (before, after) | B.null after -> [before]
                  | otherwise -> before : splitBytes separator (B.drop (B.length separator) after)

checkBuiltOutputs :: Store -> ByteString -> [(ByteString, ByteString)] -> Bool -> IO ()
checkBuiltOutputs store drvPath replacements retain = forM_ replacements $ \(original, temporary) -> do
  exists <- Files.fileExist temporary
  unless exists $ throwIO (storeError ("builder for `" ++ C.unpack drvPath ++ "' did not produce output `" ++ C.unpack original ++ "'"))
  _ <- canonicaliseAndHash store temporary
  valid <- isValidPath store original
  if not valid
    then deleteTree temporary
    else do
      expected <- queryPathHash store original
      actual <- hashNar temporary
      if expected == hashDigest actual
        then deleteTree temporary
        else do
          when retain $ do
            deleteTree (original <> "-check")
            Files.rename temporary (original <> "-check")
          unless retain $ deleteTree temporary
          throwIO (storeError ("derivation `" ++ C.unpack drvPath ++ "' may not be deterministic: output `" ++ C.unpack original ++ "' differs"))

buildLogPath :: Store -> ByteString -> IO RawFilePath
buildLogPath store drvPath = do
  configured <- getEnv "GUIX_LOG_DIRECTORY"
  let base = case configured of
        Just path -> path
        Nothing -> maybe "/tmp" (<> "/log/guix") (storeStateDirectory store)
      basename = B.drop (B.length (storeDirectory store) + 1) drvPath
  pure (base <> "/drvs/" <> B.take 2 basename <> "/" <> B.drop 2 basename)

copyLog :: Int -> Bool -> Handle -> Handle -> BuildTrace -> IO ()
copyLog pid multiplexed input output trace = do
  bytes <- B.hGetSome input 32768
  unless (B.null bytes) $ do
    B.hPutStr output bytes
    let prefix = if multiplexed
          then "@ build-log " <> C.pack (show pid) <> " " <> C.pack (show (B.length bytes)) <> "\n"
          else B.empty
    traceOutput trace (prefix <> bytes)
    copyLog pid multiplexed input output trace

builderEnvironment :: Store -> Settings -> RawFilePath -> Derivation -> IO [(String, String)]
builderEnvironment store settings work drv = do
  inherited <- getEnvironment
  let base = filter (\(key, _) -> key `elem` ["PATH", "HOME", "TMPDIR", "NIX_STORE", "NIX_STORE_DIR", "GUIX", "GUIX_UNINSTALLED", "GUIX_STATE_DIRECTORY", "GUIX_DATABASE_DIRECTORY", "GUIX_LOG_DIRECTORY", "GUILE_LOAD_PATH", "GUILE_LOAD_COMPILED_PATH", "GUIX_CONFIGURATION_DIRECTORY", "GUIX_ALLOW_UNAUTHENTICATED_SUBSTITUTES", "GUIX_BINARY_SUBSTITUTE_URL", "XDG_CACHE_HOME"]) inherited
      custom = map (\(key, value) -> (C.unpack key, C.unpack value)) (Map.toList (environment drv))
      outputsEnv = [(C.unpack name, C.unpack (outputPath output)) | (name, output) <- Map.toList (outputs drv)]
      buildEnv = [("NIX_STORE", C.unpack (storeDirectory store)), ("NIX_BUILD_TOP", C.unpack work), ("NIX_BUILD_CORES", show (settingInt "build-cores" 0 settings)), ("TMPDIR", C.unpack work), ("TEMP", C.unpack work), ("TMP", C.unpack work), ("out", C.unpack (outputPath (outputs drv Map.! "out")))]
  pure (base ++ custom ++ outputsEnv ++ buildEnv)

registerBuiltOutputs :: Store -> ByteString -> Derivation -> IO ()
registerBuiltOutputs store drvPath drv = forM_ (Map.elems (outputs drv)) $ \output -> do
  let path = outputPath output
  exists <- Files.fileExist path
  unless exists $ throwIO (storeError ("builder for `" ++ C.unpack drvPath ++ "' did not produce output `" ++ C.unpack path ++ "'"))
  _ <- canonicaliseAndHash store path
  when (not (B.null (outputHash output))) $ verifyFixedOutput output path
  (refs, hash) <- scanOutputReferences store path
  size <- fromIntegral . B.length <$> dumpNar path
  registerPath store path hash size refs drvPath
  where
    scanOutputReferences store path = do
      valid <- queryAllValidPaths store
      (found, hash) <- scanReferences path valid
      pure (Set.toList (Set.delete path found), hash)

verifyFixedOutput :: DerivationOutput -> RawFilePath -> IO ()
verifyFixedOutput output path = do
  algorithm <- either (throwIO . storeError) pure (algorithmFromName (outputHashAlgo output))
  when (algorithm /= SHA256) $ throwIO (storeError "only SHA-256 fixed-output checks are implemented")
  hash <- hashNar path
  expected <- either (throwIO . storeError) pure (decodeBase16 (outputHash output))
  when (hashDigest hash /= expected) $
    throwIO (storeError ("output `" ++ C.unpack path ++ "' has the wrong hash"))


makeBuildDirectory :: Store -> IO RawFilePath
makeBuildDirectory store = do
  let parent = maybe "/tmp" (<> "/build") (storeStateDirectory store)
  mkdirParents parent
  pid <- getProcessID
  stamp <- getPOSIXTime
  let path = parent <> "/goonix-build-" <> C.pack (show pid) <> "-" <> C.pack (show (floor stamp :: Integer))
  deleteTree path
  Dir.createDirectory path 0o700
  pure path

removeBuildDirectory :: RawFilePath -> IO ()
removeBuildDirectory path = deleteTree path
