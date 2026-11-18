{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Goonix.Store.Server
  ( serve
  , serveSocket
  , serveListener
  , activationSockets
  , handleConnection
  , ServerConfig (..)
  , defaultConfig
  , readExactly
  , writeFrame
  ) where

import Control.Concurrent (forkFinally)
import Control.Exception (IOException, SomeException, bracket, catch, throwIO, try)
import Control.Monad (forever, unless, when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import Goonix.Store.Base32 (encodeBase16)
import Goonix.Store.Config (ServerConfig (..), defaultConfig)
import Goonix.Store.ReadOnly
import Goonix.Store.Settings
import Goonix.Store.Socket
import Goonix.Store.Substitute
import Goonix.Store.Wire
import System.Posix.Env.ByteString (getEnv)
import System.Posix.Process (getProcessID)
import System.Posix.User (getRealUserID)
import System.IO (hPutStrLn, stderr)

serve :: ServerConfig -> IO ()
serve config = do
  socketPath <- getEnv "GUIX_DAEMON_SOCKET"
  let path = maybe (configStateDirectory config `orElseBytes` "/var/guix" <> "/daemon-socket/socket") id socketPath
  serveSocket config path

serveSocket :: ServerConfig -> ByteString -> IO ()
serveSocket config path = do
  listener <- openListener path
  serveListener config listener

serveListener :: ServerConfig -> Socket -> IO ()
serveListener config listener = forever $ do
  client <- acceptSocket listener
  _ <- forkFinally (handleClient config client) (const (closeSocket client))
  pure ()

activationSockets :: IO [Socket]
activationSockets = do
  listenPid <- getEnv "LISTEN_PID"
  listenFds <- getEnv "LISTEN_FDS"
  pid <- getProcessID
  case (listenPid, listenFds >>= readMaybeInt . C.unpack) of
    (Just value, Just count) | value == C.pack (show pid) && count > 0 -> do
      let sockets = map (fromFd . fromIntegral) [3 .. 2 + count]
      pure sockets
    _ -> pure []

readMaybeInt :: String -> Maybe Int
readMaybeInt value = case reads value of
  [(number, "")] -> Just number
  _ -> Nothing

openListener :: ByteString -> IO Socket
openListener path
  | not (B.isPrefixOf "/" path) = case breakLastColon path of
      Just (host, port) -> listenTcp (C.unpack host) (C.unpack port)
      Nothing -> listenTcp (C.unpack path) "44146"
  | otherwise = listenUnix path

breakLastColon :: ByteString -> Maybe (ByteString, ByteString)
breakLastColon path = case B.break (== 58) (B.reverse path) of
  (reversedPort, rest) | not (B.null rest) -> Just (B.reverse (B.drop 1 rest), B.reverse reversedPort)
  _ -> Nothing

handleClient :: ServerConfig -> Socket -> IO ()
handleClient config client = do
  isTcpClient <- isTcp client
  credential <- peerUid client
  uid <- getRealUserID
  let isTrusted = not isTcpClient && maybe False (\peer -> peer == 0 || fromIntegral peer == uid) credential
      settings = (configDefaultSettings config) { trusted = isTrusted, remote = isTcpClient }
  settingsRef <- newIORef settings
  catch
    (bracket (openStore (configStore config) (configDatabase config)) closeStore $ \store ->
      catch (handleConnection config store { storeStateDirectory = configStateDirectory config } client settingsRef) report)
    report
  where
    report exception = hPutStrLn stderr ("guix-daemon: " ++ show (exception :: SomeException))

handleConnection :: ServerConfig -> Store -> Socket -> IORef Settings -> IO ()
handleConnection config store client settingsRef = do
  magic <- readIntSocket client
  unless (magic == workerMagic1) (throwIO (userError "protocol mismatch"))
  writeSocket client (encodeInt workerMagic2 <> encodeInt protocolVersion)
  clientVersion <- readIntSocket client
  let minor = clientVersion .&. 0xff
  when (minor >= 14) $ do
    affinity <- readIntSocket client
    when (affinity /= 0) (void (readIntSocket client))
  when (minor >= 11) (void (readIntSocket client))
  startup <- try (writeFrame client stderrLast)
  case startup of
    Left exception -> throwIO (exception :: IOException)
    Right () -> loop clientVersion
  where
    loop clientVersion = do
      next <- try (readIntSocket client)
      case next of
        Left (_ :: IOError) -> pure ()
        Right operation -> do
          result <- try (dispatch config store client settingsRef clientVersion operation)
          case result of
            Left (StoreError message status) -> do
              writeSocket client (encodeInt stderrError <> encodeString message <> if clientVersion .&. 0xff >= 8 then encodeInt status else B.empty)
              loop clientVersion
            Right () -> loop clientVersion

dispatch :: ServerConfig -> Store -> Socket -> IORef Settings -> Word32 -> Word32 -> IO ()
dispatch config store client settingsRef clientVersion operation = case operation of
  19 -> do
    settings <- readIORef settingsRef
    fields <- readSetOptions client clientVersion
    case applySetOptions (clientVersion .&. 0xff) fields settings of
      Left message -> throwIO (StoreError (C.pack message) 1)
      Right updated -> writeIORef settingsRef updated
    writeFrame client stderrLast
  1 -> queryOnePath $ \_ path -> do
    _ <- assertStorePath store path
    encodeBool <$> isValidPath store path
  4 -> queryOnePath $ \_ path -> encodeString . encodeBase16 <$> queryPathHash store path
  5 -> queryOnePath $ \_ path -> encodeStrings <$> queryReferences store path
  6 -> queryOnePath $ \_ path -> encodeStrings <$> queryReferrers store path
  14 -> queryAll $ \_ -> encodeStringPairs <$> findRoots store
  18 -> queryOnePath $ \_ path -> encodeString <$> queryDeriver store path
  22 -> queryOnePath $ \_ path -> encodeStrings <$> queryDerivationOutputs store path
  23 -> queryAll $ \_ -> encodeStrings <$> queryAllValidPaths store
  24 -> queryAll $ \_ -> encodeStrings <$> queryFailedPaths store
  26 -> queryOnePath $ \_ path -> do
    info <- queryPathInfo store path
    pure (encodeString (infoDeriver info) <> encodeString (encodeBase16 (infoHash info))
      <> encodeStrings (infoReferences info) <> encodeInt (fromIntegral (infoRegistrationTime info))
      <> encodeWord64 (infoNarSize info))
  28 -> queryOnePath $ \_ path -> encodeStrings <$> queryDerivationOutputNames store path
  29 -> do
    hashPart <- readStringSocket client
    path <- queryPathFromHashPart store hashPart
    writeFrame client stderrLast
    writeSocket client (encodeString path)
  3 -> do
    path <- readStringSocket client
    _ <- assertStorePath store path
    settings <- readIORef settingsRef
    result <- if settingBool "build-use-substitutes" False settings
      then substitutablePaths config settings [path] else pure []
    writeFrame client stderrLast
    writeSocket client (encodeBool (path `elem` result))
  30 -> do
    path <- readStringSocket client
    _ <- assertStorePath store path
    settings <- readIORef settingsRef
    info <- if settingBool "build-use-substitutes" False settings
      then substitutableInfo config settings path else pure Nothing
    writeFrame client stderrLast
    writeSocket client $ case info of
      Nothing -> encodeInt 0
      Just (deriver, references, downloadSize, narSize) -> encodeInt 1 <> encodeString deriver
        <> encodeStrings references <> encodeWord64 downloadSize
        <> if clientVersion .&. 0xff >= 7 then encodeWord64 narSize else B.empty
  31 -> do
    paths <- readStorePathsSocket client
    result <- queryValidPaths store paths
    writeFrame client stderrLast
    writeSocket client (encodeStrings result)
  32 -> do
    paths <- readStorePathsSocket client
    settings <- readIORef settingsRef
    infos <- if settingBool "build-use-substitutes" False settings
      then substitutableInfos config settings paths else pure []
    writeFrame client stderrLast
    writeSocket client (encodeInt (fromIntegral (length infos)) <> foldMap encodeInfo infos)
  33 -> queryOnePath $ \_ path -> encodeStrings <$> queryValidDerivers store path
  80 -> do
    writeFrame client stderrLast
    writeSocket client (encodeStrings ["download", "git-download"])
  81 -> do
    settings <- readIORef settingsRef
    writeFrame client stderrLast
    writeSocket client (encodeStrings (substituteURLs settings))
  _ -> throwIO (StoreError (C.pack ("invalid operation " ++ show operation)) 1)
  where
    encodeInfo (path, deriver, references, downloadSize, narSize) = encodeString path
      <> encodeString deriver <> encodeStrings references <> encodeWord64 downloadSize <> encodeWord64 narSize
    queryAll action = do
      reply <- action store
      writeFrame client stderrLast
      writeSocket client reply
    queryOnePath action = do
      path <- readStringSocket client
      _ <- assertStorePath store path
      reply <- action store path
      writeFrame client stderrLast
      writeSocket client reply

readSetOptions :: Socket -> Word32 -> IO [ByteString]
readSetOptions client clientVersion = do
  let minor = clientVersion .&. 0xff
      base :: Int
      base = 4 + (if minor < 0x61 then 2 else 0) + (if minor >= 2 then 1 else 0)
        + (if minor >= 4 then 3 else 0) + (if minor >= 6 && minor < 0x61 then 1 else 0)
        + (if minor >= 10 then 1 else 0)
  fields <- mapM (const (readExactly client 8)) [1 .. base]
  if minor >= 12
    then do
      count :: Int <- fromIntegral <$> readIntSocket client
      pairs <- mapM (const readPair) [1 .. count]
      pure (fields <> [encodeInt (fromIntegral count)] <> concat pairs)
    else pure fields
  where
    readPair = do
      key <- readStringSocket client
      value <- readStringSocket client
      pure [key, value]

readIntSocket :: Socket -> IO Word32
readIntSocket client = do
  bytes <- readExactly client 8
  case decodeInt bytes of
    Left message -> throwIO (StoreError message 1)
    Right value -> pure value

readStringSocket :: Socket -> IO ByteString
readStringSocket client = do
  size <- fromIntegral <$> readIntSocket client
  when (size > 64 * 1024 * 1024) (throwIO (StoreError "wire string is too large" 1))
  let padded = size + paddingLength size
  contents <- readExactly client padded
  pure (B.take size contents)

readStorePathsSocket :: Socket -> IO [ByteString]
readStorePathsSocket client = do
  count :: Int <- fromIntegral <$> readIntSocket client
  paths <- mapM (const (readStringSocket client)) [1 .. count]
  pure (foldr (\path rest -> if path `elem` rest then rest else path:rest) [] paths)

readExactly :: Socket -> Int -> IO ByteString
readExactly client size
  | size < 0 = throwIO (StoreError "negative read size" 1)
  | otherwise = go size []
  where
    go 0 chunks = pure (B.concat (reverse chunks))
    go remaining chunks = do
      chunk <- readSocket client (min remaining 65536)
      if B.null chunk then throwIO (userError "unexpected end-of-file")
        else go (remaining - B.length chunk) (chunk:chunks)

writeFrame :: Socket -> Word32 -> IO ()
writeFrame client frame = writeSocket client (encodeInt frame)

void :: IO a -> IO ()
void action = action >> pure ()

orElseBytes :: Maybe ByteString -> ByteString -> ByteString
orElseBytes (Just value) _ = value
orElseBytes Nothing fallback = fallback
