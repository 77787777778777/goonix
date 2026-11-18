{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- Socket operations used by the daemon.  The implementation is in cbits;
-- this module deliberately depends only on unix and bytestring.
module Goonix.Store.Socket
  ( Socket (..)
  , listenUnix
  , listenTcp
  , acceptSocket
  , fromFd
  , closeSocket
  , readSocket
  , writeSocket
  , peerUid
  , isTcp
  , isListening
  , setCloseOnExec
  ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Foreign.C.String (CString, withCString)
import Foreign.C.Error (Errno (..), getErrno)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.IO.Error (userError)
import System.Posix.ByteString (RawFilePath)
import System.Posix.IO (closeFd, setFdOption, FdOption (CloseOnExec))
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Types (Fd (..))

newtype Socket = Socket { socketFd :: Fd }
  deriving (Eq, Show)

foreign import ccall unsafe "goonix_listen_unix"
  c_listen_unix :: CString -> IO CInt
foreign import ccall unsafe "goonix_listen_tcp"
  c_listen_tcp :: CString -> CString -> IO CInt
foreign import ccall safe "goonix_accept"
  c_accept :: CInt -> IO CInt
foreign import ccall unsafe "goonix_peer_uid"
  c_peer_uid :: CInt -> Ptr CUInt -> IO CInt
foreign import ccall unsafe "goonix_is_tcp"
  c_is_tcp :: CInt -> IO CInt
foreign import ccall unsafe "goonix_is_listening"
  c_is_listening :: CInt -> IO CInt

listenUnix :: RawFilePath -> IO Socket
listenUnix path = Socket . Fd <$> do
  fd <- B.useAsCString path c_listen_unix
  if fd < 0 then socketError "cannot bind/listen on Unix socket" else pure fd

listenTcp :: String -> String -> IO Socket
listenTcp host service = Socket . Fd <$> withCString host (\host' -> withCString service $ \service' -> do
  fd <- c_listen_tcp host' service'
  if fd < 0 then socketError "cannot bind/listen on TCP socket" else pure fd)

acceptSocket :: Socket -> IO Socket
acceptSocket (Socket (Fd fd)) = do
  client <- c_accept fd
  if client < 0 then do
    errno <- getErrno
    socketError ("cannot accept client: errno=" ++ show (case errno of Errno number -> number)) else pure (Socket (Fd client))

fromFd :: Fd -> Socket
fromFd = Socket

closeSocket :: Socket -> IO ()
closeSocket (Socket fd) = closeFd fd

readSocket :: Socket -> Int -> IO ByteString
readSocket (Socket fd) size = fdRead fd (fromIntegral size)

writeSocket :: Socket -> ByteString -> IO ()
writeSocket socket bytes
  | B.null bytes = pure ()
  | otherwise = do
      written <- fdWrite (socketFd socket) bytes
      if written == 0 then socketError "cannot write to socket"
        else writeSocket socket (B.drop (fromIntegral written) bytes)

peerUid :: Socket -> IO (Maybe CUInt)
peerUid (Socket (Fd fd)) = alloca $ \uid -> do
  result <- c_peer_uid fd uid
  if result == 0 then Just <$> peek uid else pure Nothing

isTcp :: Socket -> IO Bool
isTcp (Socket (Fd fd)) = (== 1) <$> c_is_tcp fd

isListening :: Socket -> IO Bool
isListening (Socket (Fd fd)) = (== 1) <$> c_is_listening fd

setCloseOnExec :: Socket -> Bool -> IO ()
setCloseOnExec (Socket fd) enabled = setFdOption fd CloseOnExec enabled

socketError :: String -> IO a
socketError message = throwIO (userError message)
