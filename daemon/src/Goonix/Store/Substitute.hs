{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Goonix.Store.Substitute
  ( SubstitutableInfo
  , substitutablePaths
  , substitutableInfo
  , substitutableInfos
  ) where

import Control.Exception (SomeException, bracket, catch, throwIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Goonix.Store.ReadOnly (StoreError (..), storeError)
import Goonix.Store.Config (ServerConfig (..))
import Goonix.Store.Settings
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hGetLine, hPutStrLn, hFlush)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, terminateProcess, waitForProcess)

type SubstitutableInfo = (ByteString, ByteString, [ByteString], Word64, Word64)

data Agent = Agent Handle Handle ProcessHandle

withAgent :: ServerConfig -> Settings -> (Agent -> IO a) -> IO a
withAgent config settings action = bracket start stop action
  where
    start = do
      let program = C.unpack (configGuixProgram config)
          environment = Just [("_NIX_OPTIONS", C.unpack (packSettings settings))]
      (input, output, _, process) <- createProcess (proc program ["substitute", "--query"])
        { std_in = CreatePipe, std_out = CreatePipe, env = environment }
      case (input, output) of
        (Just input', Just output') -> pure (Agent input' output' process)
        _ -> throwIO (storeError "cannot create substitute pipes")
    stop (Agent input output process) = do
      hClose input `catchAny` pure ()
      hClose output `catchAny` pure ()
      terminateProcess process
      _ <- waitForProcess process
      pure ()

packSettings :: Settings -> ByteString
packSettings settings = B.concat [key <> "=" <> value <> "\n" | (key, value) <- Map.toAscList (values settings)]

substitutablePaths :: ServerConfig -> Settings -> [ByteString] -> IO [ByteString]
substitutablePaths config settings paths
  | null paths = pure []
  | otherwise = withAgent config settings $ \agent -> do
      sendLine agent ("have" <> foldMap (" " <>) paths)
      readPaths agent []

substitutableInfo :: ServerConfig -> Settings -> ByteString -> IO (Maybe (ByteString, [ByteString], Word64, Word64))
substitutableInfo config settings path = do
  infos <- substitutableInfos config settings [path]
  pure $ case infos of
    [(_, deriver, references, downloadSize, narSize)] -> Just (deriver, references, downloadSize, narSize)
    _ -> Nothing

substitutableInfos :: ServerConfig -> Settings -> [ByteString] -> IO [SubstitutableInfo]
substitutableInfos config settings paths
  | null paths = pure []
  | otherwise = withAgent config settings $ \agent -> do
      sendLine agent ("info" <> foldMap (" " <>) paths)
      readInfos agent []

readPaths :: Agent -> [ByteString] -> IO [ByteString]
readPaths agent acc = do
  line <- receiveLine agent
  if B.null line then pure (reverse acc) else readPaths agent (line:acc)

readInfos :: Agent -> [SubstitutableInfo] -> IO [SubstitutableInfo]
readInfos agent acc = do
  path <- receiveLine agent
  if B.null path then pure (reverse acc) else do
    deriver <- receiveLine agent
    count <- readCount agent
    references <- mapM (const (receiveLine agent)) [1 .. count]
    downloadSize <- readCount agent
    narSize <- readCount agent
    readInfos agent ((path, deriver, references, fromIntegral downloadSize, fromIntegral narSize):acc)

sendLine :: Agent -> ByteString -> IO ()
sendLine (Agent input _ _) line = do
  B.hPutStr input line
  B.hPutStr input "\n"
  hFlush input

receiveLine :: Agent -> IO ByteString
receiveLine (Agent _ output _) = C.hGetLine output

readCount :: Agent -> IO Int
readCount agent = do
  line <- receiveLine agent
  case reads (C.unpack line) of
    [(number, "")] -> pure number
    _ -> throwIO (storeError ("integer expected from stream: " ++ C.unpack line))

catchAny :: IO a -> IO a -> IO a
catchAny action fallback = catch action (\(_ :: SomeException) -> fallback)
