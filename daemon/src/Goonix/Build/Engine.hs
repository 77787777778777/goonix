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
  , ensureStorePath
  , BuildTrace (..)
  ) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicWriteIORef)
import Data.Time.Clock.POSIX (POSIXTime)
import Control.Exception (SomeException, bracket, bracketOnError, catch, finally, fromException, throwIO, try)
import Control.Monad ((>=>), foldM, forM, forM_, replicateM_, unless, void, when)
import Data.Bits ((.|.))
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
import Goonix.Store.Path (storePathToName)
import Goonix.Store.ReadOnly
import Goonix.Store.References (scanReferences)
import Goonix.Store.Settings
import Goonix.Store.Config (ServerConfig, configGuixProgram)
import Goonix.Store.Substitute (substitutableInfos, substitutePath)
import Goonix.Store.Writing
import System.Posix.ByteString (RawFilePath)
import qualified System.Posix.Directory.ByteString as Dir
import qualified System.Posix.Files.ByteString as Files
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.IO (Handle, hClose, hPutStrLn, stderr)
import System.Process (CreateProcess (..), CmdSpec (..), StdStream (..), createProcess, getPid, proc, waitForProcess)
import System.Posix.Process (getProcessID, getProcessStatus, ProcessStatus (Exited, Terminated, Stopped))
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Posix.IO.ByteString (createPipe, fdToHandle)
import System.Posix.User (getRealGroupID, getRealUserID)
import System.FilePath (takeDirectory)
import System.Directory (findExecutable)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Types (Fd (..))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import Goonix.Build.Sandbox
import Goonix.Build.Offload
import Goonix.Build.Seccomp (nativeArchitecture, slirpSeccompFilter)
import System.Environment (getEnvironment, lookupEnv)
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
  buildThingsInternal Nothing rootFd store settings trace Nothing things mode

buildThingsWithConfig :: ServerConfig -> Fd -> Store -> Settings -> BuildTrace -> [ByteString] -> BuildMode -> IO ()
buildThingsWithConfig config rootFd store settings trace things mode = do
  program <- if settingBool "build-use-build-hook" True settings then offloadProgram config else pure Nothing
  pool <- forM program $ \path -> do
    hook <- newIORef Nothing
    pure (hook, path)
  bracket (pure pool) (mapM_ (readIORef . fst >=> mapM_ stopHook)) $ \hooks ->
    buildThingsInternal (Just config) rootFd store settings trace hooks things mode

type HookPool = (IORef (Maybe Hook), ByteString)

buildThingsInternal :: Maybe ServerConfig -> Fd -> Store -> Settings -> BuildTrace -> Maybe HookPool -> [ByteString] -> BuildMode -> IO ()
buildThingsInternal config rootFd store settings trace hook things mode = do
  failure <- newIORef Nothing
  forM_ things $ \thing ->
    catch (buildOne thing) $ \(exception :: SomeException) ->
      if keepGoing settings
        then do
          previous <- readIORef failure
          when (previous == Nothing) $ writeIORef failure . Just =<< case fromException exception of
            Just (StoreError message _) -> pure (StoreError message 100)
            Nothing -> pure (StoreError (C.pack (show exception)) 100)
        else throwIO exception
  readIORef failure >>= mapM_ throwIO
  where
    buildOne thing
      | B.isSuffixOf ".drv" thing = buildDerivation config rootFd store settings trace hook mode thing
      | B.isInfixOf "!" thing = do
          let (drv, _) = B.break (== 33) thing
          buildOne drv
      | otherwise = case config of
          Just serverConfig -> void (trySubstitute serverConfig rootFd store settings trace thing)
          Nothing -> pure ()

ensureStorePath :: ServerConfig -> Fd -> Store -> Settings -> BuildTrace -> ByteString -> IO ()
ensureStorePath config rootFd store settings trace path = do
  substituted <- trySubstitute config rootFd store settings trace path
  unless substituted $ throwIO (storeError ("path `" ++ C.unpack path ++ "' does not exist and cannot be created"))

buildThings :: Fd -> Store -> Settings -> [ByteString] -> BuildMode -> IO ()
buildThings rootFd store settings things mode =
  buildThingsWithTrace rootFd store settings (BuildTrace (\_ _ _ _ -> pure ()) (\_ -> pure ()) (\_ _ -> pure ()) (\_ -> pure ())) things mode

buildDerivation :: Maybe ServerConfig -> Fd -> Store -> Settings -> BuildTrace -> Maybe HookPool -> BuildMode -> ByteString -> IO ()
buildDerivation config rootFd store settings trace hook mode drvPath = do
  appendTempRoot rootFd drvPath
  valid <- isValidPath store drvPath
  unless valid $ case config of
    Just serverConfig | mode == Normal -> do
      -- As in DerivationGoal::init, a missing derivation may itself be
      -- substitutable.  Failure to find it is reported by the caller.
      _ <- trySubstitute serverConfig rootFd store settings trace drvPath
      pure ()
    _ -> pure ()
  stillValid <- isValidPath store drvPath
  unless stillValid $ throwIO (storeError ("cannot build missing derivation `" ++ C.unpack drvPath ++ "'"))
  bytes <- B.readFile (C.unpack drvPath)
  drv <- either (throwIO . storeError) pure (parseDerivation bytes)
  forM_ (Map.keys (inputDrvs drv)) $ \input -> buildDerivation config rootFd store settings trace hook Normal input
  forM_ (Set.toList (inputSrcs drv)) $ \input -> do
    present <- isValidPath store input
    unless present $ throwIO (storeError ("input `" ++ C.unpack input ++ "' is not valid"))
  ready <- allOutputsValid store drv
  failed <- if mode == Normal && not ready && settingBool "build-cache-failures" False settings
    then or <$> mapM (hasPathFailed store . outputPath) (Map.elems (outputs drv))
    else pure False
  when failed $ do
    traceFailed trace drvPath "cached"
    throwIO (storeError ("build of `" ++ C.unpack drvPath ++ "' failed previously (cached)"))
  substituted <- case config of
    Just serverConfig | mode /= Check && not ready && settingBool "build-use-substitutes" False settings ->
      or <$> mapM (trySubstitute serverConfig rootFd store settings trace . outputPath) (Map.elems (outputs drv))
    _ -> pure False
  when (mode == Repair || mode == Check || not (ready || substituted)) $
    buildWithHook rootFd store settings trace hook mode drvPath drv

buildWithHook :: Fd -> Store -> Settings -> BuildTrace -> Maybe HookPool -> BuildMode -> ByteString -> Derivation -> IO ()
buildWithHook rootFd store settings trace pool mode drvPath drv = do
  let localBuild = do
        runBuilder rootFd store settings trace mode drvPath drv
        when (mode == Normal) $ replicateM_ (max 0 (settingInt "build-repeat" 0 settings)) $
          runBuilder rootFd store settings trace Check drvPath drv
      withOutputLocks action =
        foldr (\path inner -> withPathLock path inner) action (map outputPath (Map.elems (outputs drv)))
      tryBuild Nothing = runBuilder rootFd store settings trace mode drvPath drv
      tryBuild (Just currentPool)
        | mode /= Normal || willBuildLocally settings drv = localBuild
        | otherwise = withOutputLocks $ bracketOnError (acquireHook settings currentPool)
            (\hook -> releaseHook currentPool hook >> stopHook hook) $ \hook ->
            offer currentPool hook
      offer currentPool hook = do
        ready <- allOutputsValid store drv
        if ready then releaseHook currentPool hook else do
          -- C++ holds the output locks while offering the job to the hook.
          reply <- requestOffload hook (traceOutput trace) True (platform drv) drvPath
            (C.words (Map.findWithDefault "" "requiredSystemFeatures" (environment drv)))
          case reply of
            HookDecline -> releaseHook currentPool hook >> localBuild
            HookPostpone -> do
              -- waitForAWhile: keep the same live hook for the next offer.
              threadDelay 5000000
              offer currentPool hook
            HookAccept -> do
              -- An accepted hook owns the build.  In particular, a helper
              -- failure is not a request to retry locally.
              inputRoots <- Set.toList <$> foldM (derivationInputs store) Set.empty [drv]
              inputs <- Set.toList . Set.fromList . concat <$> mapM (referenceClosure store) (drvPath:inputRoots)
              let outputPaths = map outputPath (Map.elems (outputs drv))
              logPath <- buildLogPath settings store drvPath
              mkdirParents (C.pack (takeDirectory (C.unpack logPath)))
              traceStarted trace drvPath (platform drv) logPath (fromIntegral (hookPid hook))
              acceptOffload hook inputs outputPaths
              bracket (openLog settings logPath) closeLog $ \logHandle -> do
                outputLock <- newMVar ()
                let emit bytes = withMVar outputLock $ \_ -> writeLog logHandle bytes >> traceOutput trace bytes
                status <- finishOffload hook emit
                releaseHook currentPool hook
                valid <- and <$> mapM (isValidPath store) outputPaths
                case (status, valid) of
                  (HelperExited 0, True) -> traceSucceeded trace drvPath
                  _ -> offloadFailed settings store trace drvPath drv status
  tryBuild pool

acquireHook :: Settings -> HookPool -> IO Hook
acquireHook settings (state, program) = do
  existing <- readIORef state
  case existing of
    Just hook -> do
      writeIORef state Nothing
      pure hook
    Nothing -> startHelper settings program

releaseHook :: HookPool -> Hook -> IO ()
releaseHook (state, _) = writeIORef state . Just

willBuildLocally :: Settings -> Derivation -> Bool
willBuildLocally settings drv =
  Map.findWithDefault "" "preferLocalBuild" (environment drv) == "1" &&
    (platform drv == system || (platform drv == "i686-linux" && system == "x86_64-linux") ||
      (platform drv == "armhf-linux" && system == "aarch64-linux"))
  where system = setting "system" "x86_64-linux" settings

offloadProgram :: ServerConfig -> IO (Maybe ByteString)
offloadProgram config = do
  configured <- getEnv "GUIX"
  override <- getEnv "GOONIX_OFFLOAD_PROGRAM"
  pure $ Just $ case override of
    Just path -> path
    Nothing -> maybe (configGuixProgram config) id configured

startHelper :: Settings -> ByteString -> IO Hook
startHelper settings = \program -> startOffloadHook program (setting "system" "x86_64-linux" settings)
  (settingInt "build-max-silent-time" 3600 settings) (printBuildTrace settings) (settingInt "build-timeout" 86400 settings)

offloadFailed :: Settings -> Store -> BuildTrace -> ByteString -> Derivation -> HelperStatus -> IO a
offloadFailed settings store trace drvPath drv status = do
  let statusCode = case status of
        HelperExited code | code `elem` [100, 101] -> fromIntegral code
        _ -> 1
      reason = case status of
        HelperExited 0 -> "failed to produce all outputs"
        HelperExited code -> "failed with exit code " ++ show code
        HelperSignalled signal -> "failed due to signal " ++ show signal
        HelperAbnormal -> "died abnormally"
      failure = "offload helper for `" ++ C.unpack drvPath ++ "' " ++ reason
  case status of
    HelperExited 101 -> traceFailed trace drvPath "timeout"
    HelperExited 100 -> traceFailed trace drvPath failure
    HelperExited 0 -> traceFailed trace drvPath failure
    _ -> pure ()
  when (statusCode == 100 && settingBool "build-cache-failures" False settings && not (isFixedOutput drv)) $
    mapM_ (registerFailedPath store . outputPath) (Map.elems (outputs drv))
  throwIO (StoreError (C.pack ("build of " ++ C.unpack drvPath ++ " failed: " ++ failure)) statusCode)

derivationInputs :: Store -> Set.Set ByteString -> Derivation -> IO (Set.Set ByteString)
derivationInputs store seen current = do
  let direct = Set.filter (\path -> not ("builtin:" `B.isPrefixOf` path))
        (if "builtin:" `B.isPrefixOf` builder current
          then inputSrcs current
          else Set.insert (topLevelStorePath store (builder current)) (inputSrcs current))
  dependencies <- mapM readStoreDerivation (filter (`Set.notMember` seen) (Map.keys (inputDrvs current)))
  let dependencyOutputs = Set.fromList [outputPath output | dependency <- dependencies, output <- Map.elems (outputs dependency)]
  foldM (derivationInputs store) (Set.union seen (Set.union direct dependencyOutputs)) dependencies
  where
    readStoreDerivation path = do
      bytes <- B.readFile (C.unpack path)
      either (throwIO . storeError) pure (parseDerivation bytes)

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
                when ("hash-mismatch" `B.isPrefixOf` message) $
                  case C.words message of
                    (_:algorithm:expected:actual:_) ->
                      traceOutput trace ("@ hash-mismatch " <> path <> " " <> algorithm <> " " <> expected <> " " <> actual <> "\n")
                    _ -> pure ()
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
                (actualHash, _) <- canonicaliseAndHash store path
                when (hashDigest actualHash /= digest) $ do
                  deleteTree path
                  throwIO (storeError ("corrupt output path `" ++ C.unpack path ++ "' from substitute"))
                deduplicateTree (storeDirectory store) path
                registerPath store path actualHash size references deriver
                traceOutput trace ("@ substituter-succeeded " <> path <> "\n")
                pure True

allOutputsValid :: Store -> Derivation -> IO Bool
allOutputsValid store drv = and <$> mapM (isValidPath store . outputPath) (Map.elems (outputs drv))

runBuilder :: Fd -> Store -> Settings -> BuildTrace -> BuildMode -> ByteString -> Derivation -> IO ()
runBuilder rootFd store settings trace mode drvPath drv = do
  let isBuiltin = "builtin:" `B.isPrefixOf` builder drv
      outPaths = map outputPath (Map.elems (outputs drv))
      redirected = if mode == Check then map (redirectOutput store drvPath) outPaths else outPaths
      replacements = zip outPaths redirected
      actualDrv = if mode == Check then replaceDerivationPaths replacements drv else drv
  mapM_ (appendTempRoot rootFd) outPaths
  mapM_ deleteTree redirected
  work <- makeBuildDirectory store
  prepareReferenceGraphs store work drv
  env <- builderEnvironment store settings work actualDrv
  logPath <- buildLogPath settings store drvPath
  mkdirParents (C.pack (takeDirectory (C.unpack logPath)))
  logHandle <- openLog settings logPath
  (readFd, writeFd) <- createPipe
  writeHandle <- fdToHandle writeFd
  readHandle <- fdToHandle readFd
  guixProgram <- maybe "guix" C.unpack <$> getEnv "GUIX"
  let command = if "builtin:" `B.isPrefixOf` builder drv
        then proc guixProgram ["perform-download", C.unpack drvPath, C.unpack (outputPath (outputs actualDrv Map.! "out"))]
        else proc (C.unpack (builder drv)) (map C.unpack (args drv))
  (childPid, wait, sandboxRoot, sandboxOutputs) <- if settingBool "build-use-sandbox" True settings && not (isBuiltin)
    then spawnSandboxedBuilder settings store work drvPath actualDrv command env writeFd
    else do
      -- As in builtins.cc, the downloader inherits the daemon's environment.
      inherited <- if isBuiltin then getEnvironment else pure []
      let childEnvironment = if isBuiltin then inherited else env
      (_, _, _, process) <- createProcess command
        { cwd = Just (C.unpack work), env = Just childEnvironment, std_out = UseHandle writeHandle, std_err = UseHandle writeHandle, create_group = True }
      pid <- maybe 0 fromIntegral <$> getPid process
      pure (pid, waitForProcess process, Nothing, [])
  traceStarted trace drvPath (platform drv) logPath childPid
  hClose writeHandle
  lastOutput <- getPOSIXTime >>= newIORef
  drained <- newEmptyMVar
  _ <- forkIO $ copyLog childPid (settingBool "multiplexed-build-output" False settings) readHandle logHandle trace lastOutput `finally` putMVar drained ()
  (status, timeoutReason) <- withWritableStore (storeDirectory store) $
    waitForBuilder settings childPid wait lastOutput
  takeMVar drained
  hClose readHandle
  closeLog logHandle
  case status of
    ExitSuccess -> do
      forM_ sandboxRoot $ \root -> forM_ sandboxOutputs $ \path -> do
        let produced = root <> path
        exists <- Files.fileExist produced
        unless exists $ throwIO (storeError ("builder for `" ++ C.unpack drvPath ++ "' failed: did not produce output `" ++ C.unpack path ++ "'"))
        deleteTree path
        makeParentsWritable root path
        copyTree produced path
      forM_ sandboxRoot $ \root -> do
        Files.setFileMode root 0o755
        deleteTree root
      registration <- try $ if mode == Check
        then checkBuiltOutputs store drvPath replacements (keepFailed settings)
        else registerBuiltOutputs store drvPath drv
      case registration of
        Left (exception :: SomeException) -> do
          forM_ redirected $ \path -> deleteTree path
          let (message, status) = registrationFailure exception
          buildFailed work (Just (message, status))
        Right () -> pure ()
      removeBuildDirectory work
      traceSucceeded trace drvPath
    _ -> do
      forM_ sandboxRoot $ \root -> do
        Files.setFileMode root 0o755
        deleteTree root
      buildFailed work ((, 101) <$> timeoutReason)
  where
    -- Output registration can fail after a successful builder.  It is not a
    -- timeout, and its StoreError status must survive rather than becoming
    -- the timeout status 101 (which would also disable the failure cache).
    registrationFailure exception =
      case fromException exception of
        Just (StoreError message status) -> (C.unpack message, status)
        Nothing -> (show exception, 1)
    buildFailed work timeoutReason = do
      let (failure, status) = case timeoutReason of
            Just reason -> reason
            Nothing -> ("builder failed", 1)
          timedOut = status == 101
          reason = "failed: " ++ failure
      traceFailed trace drvPath (if timedOut then "timeout" else "builder failed")
      when (mode == Normal && not timedOut && settingBool "build-cache-failures" False settings && not (isFixedOutput drv)) $
        mapM_ (registerFailedPath store . outputPath) (Map.elems (outputs drv))
      if keepFailed settings
        then pure ()
        else removeBuildDirectory work
      throwIO (StoreError (C.pack ("build of " ++ C.unpack drvPath ++ " " ++ reason)) status)

foreign import ccall unsafe "goonix_spawn_sandbox"
  c_spawn_sandbox :: CString -> CString -> CInt -> Ptr CInt -> IO CInt

topLevelStorePath :: Store -> RawFilePath -> RawFilePath
topLevelStorePath store path =
  storeDirectory store <> "/" <> C.takeWhile (/= '/') (B.drop (B.length (storeDirectory store) + 1) path)

makeParentsWritable :: RawFilePath -> RawFilePath -> IO ()
makeParentsWritable root path = go (C.split '/' path) root
  where
    go [] _ = pure ()
    go [_] _ = pure ()
    go (component:rest) parent = do
      let directory = parent <> "/" <> component
      Files.setFileMode directory 0o755
      go rest directory

copyTree :: RawFilePath -> RawFilePath -> IO ()
copyTree source destination = do
  status <- Files.getSymbolicLinkStatus source
  if Files.isSymbolicLink status
    then do
      target <- Files.readSymbolicLink source
      Files.createSymbolicLink target destination
    else if Files.isDirectory status
      then do
        Dir.createDirectory destination 0o755
        entries <- directoryEntries source
        forM_ entries $ \entry -> copyTree (source <> "/" <> entry) (destination <> "/" <> entry)
        Files.setFileMode destination (Files.fileMode status)
      else do
        bytes <- B.readFile (C.unpack source)
        B.writeFile (C.unpack destination) bytes
        Files.setFileMode destination (Files.fileMode status)

spawnSandboxedBuilder :: Settings -> Store -> RawFilePath -> RawFilePath -> Derivation -> CreateProcess -> [(String, String)] -> Fd -> IO (Int, IO ExitCode, Maybe RawFilePath, [RawFilePath])
spawnSandboxedBuilder settings store work drvPath drv command env (Fd logFd) = do
  helper <- maybe (storeDirectory store <> "/../guix-sandbox") id <$> getEnv "GUIX_SANDBOX"
  root <- makeSandboxRoot store
  mkdirParents (root <> "/tmp")
  Files.setFileMode (root <> "/tmp") 0o1777
  uid <- fromIntegral <$> getRealUserID
  gid <- fromIntegral <$> getRealGroupID
  network <- fixedOutputNetwork settings drv
  let (networkFlags, slirpProgram, slirpFilter) = case network of
        Nothing -> (0, "", [])
        Just (program, ipv6, isolate) ->
          (sandboxFlagSlirpNetwork .|. (if ipv6 then sandboxFlagIpv6Network else 0) .|.
            (if isolate then sandboxFlagIsolateHostLoopback else 0),
            C.pack program, slirpSeccompFilter nativeArchitecture)
  inputs <- foldM derivationInputs Set.empty [drv]
  let writableInputs = Set.fromList [work, storeDirectory store]
  closure <- Set.unions . map Set.fromList <$> mapM (referenceClosure store) (Set.toList inputs)
  let sandboxWork = "/tmp/guix-build-" <> storePathToName (storeDirectory store) drvPath <> "-0"
      program = case cmdspec command of
        RawCommand executable _ -> C.pack executable
        ShellCommand _ -> throwIO (storeError "shell builders are not supported") `seq` B.empty
      arguments = case cmdspec command of
        RawCommand executable args -> C.pack executable : map C.pack args
        ShellCommand _ -> []
      environment = map (\(name, value) -> (C.pack name, C.pack value)) env
      replaceWork path = if work `B.isPrefixOf` path then sandboxWork <> B.drop (B.length work) path else path
      environment' = map (\(name, value) -> (name, replaceWork value)) (Map.toList (Map.fromList environment))
      mountInputs = [BindMount path path (not (Set.member path writableInputs)) | path <- Set.toList closure]
      mountOutputs = []
      mountWork = [BindMount work sandboxWork False]
      devices = [BindMount path path False | path <- ["/dev/null", "/dev/full", "/dev/zero", "/dev/random", "/dev/urandom", "/dev/tty"]]
      plan = SandboxPlan
        { planFlags = sandboxFlagUserNamespace .|. sandboxFlagMountNamespace .|.
          sandboxFlagNetworkNamespace .|. sandboxFlagPidNamespace .|. sandboxFlagIpcNamespace .|.
          sandboxFlagUtsNamespace .|. sandboxFlagChroot .|. sandboxFlagProc .|. sandboxFlagDevpts .|.
          sandboxFlagLockMounts .|. sandboxFlagSetSid .|. sandboxFlagCloseFds .|.
          sandboxFlagChdir .|. sandboxFlagStdinFile .|. sandboxFlagClearGroups .|.
          sandboxFlagNoNewPrivs .|. networkFlags
        , planProgram = program, planArguments = arguments, planEnvironment = environment'
        , planWorkingDirectory = sandboxWork, planStdinFile = "/dev/null"
        , planUid = 30001, planGid = 30000, planStdinFd = maxBound, planLogFd = maxBound
        , planGroups = [], planCloseFds = []
        , planUidMapInside = 30001, planUidMapOutside = uid
        , planGidMapInside = 30000, planGidMapOutside = gid
        , planRoot = root, planStoreDirectory = storeDirectory store, planHostname = "localhost", planDomainname = "(none)"
        , planMounts = mountInputs ++ mountOutputs ++ mountWork ++ devices
        , planSeccomp = []
        , planSlirpProgram = slirpProgram, planSlirpSeccomp = slirpFilter
        }
      planPath = root <> ".plan"
  mkdirParents (root <> "/etc")
  B.writeFile (C.unpack (root <> "/etc/passwd"))
    "nixbld:x:30001:30000:Nix build user:/:/noshell\nnobody:x:65534:65534:Nobody:/:/noshell\n"
  B.writeFile (C.unpack (root <> "/etc/group")) "nixbld:!:30000:\n"
  B.writeFile (C.unpack (root <> "/etc/hosts")) "127.0.0.1 localhost\n::1 localhost\n"
  when (isFixedOutput drv) $ do
    forM_ ["/etc/services", "/etc/nsswitch.conf"] (copyEtcFile root)
    case network of
      Nothing -> forM_ ["/etc/hosts", "/etc/resolv.conf"] (copyEtcFile root)
      Just (_, _, isolate) -> do
        B.writeFile (C.unpack (root <> "/etc/resolv.conf")) "nameserver 10.0.2.3\n"
        if isolate
          then copyEtcFile root "/etc/hosts"
          else do
            hosts <- B.readFile "/etc/hosts" `catch` \(_ :: SomeException) -> pure "127.0.0.1 localhost\n::1 localhost\n"
            B.writeFile (C.unpack (root <> "/etc/hosts"))
              (replaceBytes "::1" "fd00::2" (replaceBytes "127.0.0.1" "10.0.2.2" hosts))
  B.writeFile (C.unpack planPath) (encodeSandboxPlan plan)
  withCString (C.unpack helper) $ \program' -> withCString (C.unpack planPath) $ \path' -> alloca $ \pidPointer -> do
    result <- c_spawn_sandbox program' path' logFd pidPointer
    if result < 0 then throwIO (storeError "cannot spawn sandbox helper") else do
      pid <- fromIntegral <$> peek pidPointer
      pure (pid, waitForSandbox pid, Just root, map outputPath (Map.elems (outputs drv)))
  where
    derivationInputs seen current
      | otherwise = do
          let direct = Set.filter (\path -> not ("builtin:" `B.isPrefixOf` path))
                (if "builtin:" `B.isPrefixOf` builder current
                  then inputSrcs current
                  else Set.insert (topLevelStorePath store (builder current)) (inputSrcs current))
          dependencies <- mapM readStoreDerivation (filter (`Set.notMember` seen) (Map.keys (inputDrvs current)))
          let dependencyOutputs = Set.fromList [outputPath output | dependency <- dependencies, output <- Map.elems (outputs dependency)]
          foldM derivationInputs (Set.union seen (Set.union direct dependencyOutputs)) dependencies
    readStoreDerivation path = do
      bytes <- B.readFile (C.unpack path)
      either (throwIO . storeError) pure (parseDerivation bytes)

copyEtcFile :: RawFilePath -> RawFilePath -> IO ()
copyEtcFile root path =
  (B.readFile (C.unpack path) >>= \contents -> B.writeFile (C.unpack (root <> path)) contents)
    `catch` \(_ :: SomeException) -> pure ()

fixedOutputNetwork :: Settings -> Derivation -> IO (Maybe (FilePath, Bool, Bool))
fixedOutputNetwork settings drv
  | not (isFixedOutput drv) = pure Nothing
  | otherwise = do
      configured <- lookupEnv "GUIX_SLIRP4NETNS"
      program <- case configured of
        Just path | not (null path) -> pure (Just path)
        _ -> findExecutable "slirp4netns"
      tun <- Files.fileExist "/dev/net/tun"
      case (program, tun) of
        (Just path, True) -> do
          ipv6 <- hasGlobalIpv6
          pure (Just (path, ipv6, settingBool "build-isolate-host-loopback" False settings))
        _ -> do
          hPutStrLn stderr "guix-daemon: fixed-output network access disabled (slirp4netns or /dev/net/tun unavailable)"
          pure Nothing
  where
    hasGlobalIpv6 = do
      contents <- B.readFile "/proc/net/if_inet6" `catch` \(_ :: SomeException) -> pure ""
      pure (any (\line -> case C.words line of
        (_:_:_:scope:_) -> scope == "00" || scope == "40"
        _ -> False) (C.lines contents))

waitForBuilder :: Settings -> Int -> IO ExitCode -> IORef POSIXTime -> IO (ExitCode, Maybe String)
waitForBuilder settings pid wait lastOutput = do
  started <- getPOSIXTime
  status <- newEmptyMVar
  reason <- newIORef Nothing
  _ <- forkIO (try wait >>= putMVar status)
  let total = settingInt "build-timeout" 0 settings
      silent = settingInt "build-max-silent-time" 0 settings
      stop why = do
        atomicWriteIORef reason (Just why)
        signalProcessGroup sigKILL (fromIntegral pid) `catch` \exception ->
          if isDoesNotExistError exception then pure () else throwIO exception
      watch = do
        threadDelay 100000
        now <- getPOSIXTime
        latest <- readIORef lastOutput
        if total > 0 && now - started >= fromIntegral total
          then stop "timeout"
          else if silent > 0 && now - latest >= fromIntegral silent
            then stop "silence timeout"
            else watch
  timer <- if total > 0 || silent > 0 then Just <$> forkIO watch else pure Nothing
  result <- takeMVar status
  mapM_ killThread timer
  why <- readIORef reason
  either (\(exception :: SomeException) -> throwIO exception)
    (\code -> pure (if why /= Nothing then ExitFailure 1 else code, why)) result

waitForSandbox :: Int -> IO ExitCode
waitForSandbox pid = do
  status <- getProcessStatus True False (fromIntegral pid)
  case status of
    Just (Exited code) -> pure code
    Just (Terminated _ _) -> pure (ExitFailure 1)
    Just (Stopped _) -> waitForSandbox pid
    Nothing -> throwIO (storeError "cannot wait for sandbox helper")

makeSandboxRoot :: Store -> IO RawFilePath
makeSandboxRoot store = do
  let parent = maybe "/tmp" (<> "/build") (storeStateDirectory store)
  mkdirParents parent
  pid <- getProcessID
  stamp <- getPOSIXTime
  let path = parent <> "/goonix-sandbox-" <> C.pack (show pid) <> "-" <> C.pack (show (floor stamp :: Integer))
  deleteTree path
  Dir.createDirectory path 0o755
  pure path

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

buildLogPath :: Settings -> Store -> ByteString -> IO RawFilePath
buildLogPath settings store drvPath = do
  configured <- getEnv "GUIX_LOG_DIRECTORY"
  let base = case configured of
        Just path -> path
        Nothing -> maybe "/tmp" (<> "/log/guix") (storeStateDirectory store)
      basename = B.drop (B.length (storeDirectory store) + 1) drvPath
      suffix = case setting "build-log-compression" "gzip" settings of
        "none" -> ""
        "bzip2" -> ".bz2"
        _ -> ".gz"
  pure (base <> "/drvs/" <> B.take 2 basename <> "/" <> B.drop 2 basename <> suffix)

data LogWriter = LogWriter CInt (Ptr ())

foreign import ccall unsafe "goonix_open_log"
  c_open_log :: CString -> CInt -> IO (Ptr ())
foreign import ccall safe "goonix_write_log"
  c_write_log :: Ptr () -> CInt -> Ptr a -> CSize -> IO CInt
foreign import ccall safe "goonix_close_log"
  c_close_log :: Ptr () -> CInt -> IO CInt

openLog :: Settings -> RawFilePath -> IO LogWriter
openLog settings path = do
  let kind = logKind settings
  writer <- B.useAsCString path (\path' -> c_open_log path' kind)
  if writer == nullPtr
    then throwIO (storeError ("cannot open build log `" ++ C.unpack path ++ "'"))
    else pure (LogWriter kind writer)

logKind :: Settings -> CInt
logKind settings = case setting "build-log-compression" "gzip" settings of
  "none" -> 0
  "bzip2" -> 2
  _ -> 1

writeLog :: LogWriter -> ByteString -> IO ()
writeLog (LogWriter kind writer) bytes = do
  result <- B.useAsCStringLen bytes $ \(pointer, length') ->
    c_write_log writer kind (castPtr pointer) (fromIntegral length')
  when (result < 0) (throwIO (storeError "cannot write build log"))

closeLog :: LogWriter -> IO ()
closeLog (LogWriter kind writer) = do
  result <- c_close_log writer kind
  when (result < 0) (throwIO (storeError "cannot close build log"))

copyLog :: Int -> Bool -> Handle -> LogWriter -> BuildTrace -> IORef POSIXTime -> IO ()
copyLog pid multiplexed input output trace lastOutput = do
  bytes <- B.hGetSome input 32768
  unless (B.null bytes) $ do
    getPOSIXTime >>= writeIORef lastOutput
    writeLog output bytes
    let prefix = if multiplexed
          then "@ build-log " <> C.pack (show pid) <> " " <> C.pack (show (B.length bytes)) <> "\n"
          else B.empty
    traceOutput trace (prefix <> bytes)
    copyLog pid multiplexed input output trace lastOutput

builderEnvironment :: Store -> Settings -> RawFilePath -> Derivation -> IO [(String, String)]
builderEnvironment store settings work drv = do
  let base = [("PATH", "/path-not-set"), ("HOME", "/homeless-shelter"),
              ("NIX_STORE", C.unpack (storeDirectory store)),
              ("NIX_BUILD_CORES", show (settingInt "build-cores" 0 settings))]
      custom = map (\(key, value) -> (C.unpack key, C.unpack value)) (Map.toList (environment drv))
      buildEnv = [("NIX_BUILD_TOP", C.unpack work), ("TMPDIR", C.unpack work),
                  ("TEMP", C.unpack work), ("TMP", C.unpack work),
                  ("TEMPDIR", C.unpack work), ("PWD", C.unpack work)]
  leaked <- if isFixedOutput drv
    then mapM (\name -> (C.unpack name,) . maybe "" C.unpack <$> getEnv name)
         (C.words (Map.findWithDefault "" "impureEnvVars" (environment drv)))
    else pure []
  pure (Map.toList (Map.fromList (base ++ custom ++ leaked ++ buildEnv)))

registerBuiltOutputs :: Store -> ByteString -> Derivation -> IO ()
registerBuiltOutputs store drvPath drv = forM_ (Map.elems (outputs drv)) $ \output -> do
  let path = outputPath output
  exists <- Files.fileExist path
  unless exists $ throwIO (storeError ("builder for `" ++ C.unpack drvPath ++ "' failed: did not produce output `" ++ C.unpack path ++ "'"))
  _ <- canonicaliseAndHash store path
  when (not (B.null (outputHash output))) $ verifyFixedOutput output path
  deduplicateTree (storeDirectory store) path
  (refs, hash) <- scanOutputReferences store path
  enforceReferencePolicy store drv path refs
  size <- fromIntegral . B.length <$> dumpNar path
  registerPath store path hash size (filter (/= path) refs) drvPath
  where
    scanOutputReferences store path = do
      valid <- queryAllValidPaths store
      (found, hash) <- scanReferences path (path : valid)
      pure (Set.toList found, hash)

verifyFixedOutput :: DerivationOutput -> RawFilePath -> IO ()
verifyFixedOutput output path = do
  algorithm <- either (throwIO . storeError) pure (algorithmFromName (if "r:" `B.isPrefixOf` outputHashAlgo output then B.drop 2 (outputHashAlgo output) else outputHashAlgo output))
  expected <- case decodeBase16 (outputHash output) of
    Right digest | B.length digest == digestLength algorithm -> pure digest
    _ -> throwIO (storeError ("invalid " ++ C.unpack (C.map toUpperAscii (algorithmName algorithm)) ++ " hash"))
  hash <- if "r:" `B.isPrefixOf` outputHashAlgo output
    then hashNar path
    else hashFile algorithm path
  when (hashDigest hash /= expected) $
    throwIO (storeError ("output `" ++ C.unpack path ++ "' has the wrong hash"))
  where
    toUpperAscii character
      | character >= 'a' && character <= 'z' = toEnum (fromEnum character - 32)
      | otherwise = character

withWritableStore :: RawFilePath -> IO a -> IO a
withWritableStore store = bracket prepare restore . const
  where
    prepare = do
      status <- Files.getSymbolicLinkStatus store
      Files.setFileMode store (Files.fileMode status .|. 0o700)
      pure (Files.fileMode status)
    restore = Files.setFileMode store

prepareReferenceGraphs :: Store -> RawFilePath -> Derivation -> IO ()
prepareReferenceGraphs store work drv =
  case Map.lookup "exportReferencesGraph" (environment drv) of
    Nothing -> pure ()
    Just fields -> go (C.words fields)
      where
        go [] = pure ()
        go (name:path:rest) = do
          let root = topLevelStorePath store path
          initial <- referenceClosure store root
          withOutputs <- foldM includeDerivationOutputs initial initial
          entries <- forM withOutputs $ \item -> do
            info <- queryPathInfo store item
            pure (item <> "\n\n" <> C.pack (show (length (infoReferences info))) <> "\n"
                  <> B.intercalate "\n" (infoReferences info)
                  <> if null (infoReferences info) then "" else "\n")
          B.writeFile (C.unpack (work <> "/" <> name)) (B.concat entries)
          go rest
        go _ = throwIO (storeError "odd number of tokens in exportReferencesGraph")
        includeDerivationOutputs result item
          | not (".drv" `B.isSuffixOf` item) = pure result
          | otherwise = do
              bytes <- B.readFile (C.unpack item)
              parsed <- either (throwIO . storeError) pure (parseDerivation bytes)
              expanded <- mapM (referenceClosure store . outputPath) (Map.elems (outputs parsed))
              pure (Set.toAscList (Set.fromList (result ++ concat expanded)))

referenceClosure :: Store -> RawFilePath -> IO [RawFilePath]
referenceClosure store root = go Set.empty [root]
  where
    go seen [] = pure (Set.toAscList seen)
    go seen (path:rest)
      | Set.member path seen = go seen rest
      | otherwise = do
          references <- queryReferences store path
          go (Set.insert path seen) (rest ++ references)

enforceReferencePolicy :: Store -> Derivation -> RawFilePath -> [RawFilePath] -> IO ()
enforceReferencePolicy store drv path references = do
  forM_ ["allowedReferences", "allowedRequisites", "disallowedReferences", "disallowedRequisites"] $ \attribute ->
    forM_ (Map.lookup attribute (environment drv)) $ \specification -> do
      let allowed = attribute `elem` ["allowedReferences", "allowedRequisites"]
          recursive = attribute `elem` ["allowedRequisites", "disallowedRequisites"]
      specified <- foldM expand Set.empty (C.words specification)
      used <- if recursive
        then Set.fromList . concat <$> mapM (referenceClosure store) references
        else pure (Set.fromList references)
      forM_ (Set.toList used) $ \reference ->
        when (Set.member reference specified /= allowed) $
          throwIO (storeError ("output (`" ++ C.unpack path ++ "') is not allowed to refer to path `" ++ C.unpack reference ++ "'"))
  where
    expand specified token
      | token `elem` Map.keys (outputs drv) = pure (Set.insert (outputPath (outputs drv Map.! token)) specified)
      | otherwise = do
          valid <- isValidPath store token
          if valid then pure (Set.insert token specified)
            else throwIO (storeError "invalid reference specifier")


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
