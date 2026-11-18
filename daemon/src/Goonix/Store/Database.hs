{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- Small SQLite FFI for the daemon's exact schema.  No text conversion is
-- used for paths: SQLite values are copied as bytes, as in the C++ daemon.
module Goonix.Store.Database
  ( Database
  , Statement
  , Value (..)
  , openReadOnly
  , closeDatabase
  , withReadOnly
  , prepare
  , closeStatement
  , withStatement
  , bindValues
  , query
  , queryValues
  , queryRows
  , databaseError
  ) where

import Control.Exception (bracket, bracketOnError, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CLLong (..), CUChar (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, castPtrToFunPtr, intPtrToPtr, nullPtr)
import Foreign.Storable (peek)

newtype Database = Database (Ptr ())
newtype Statement = Statement (Ptr ())

data Value = Integer !Int64 | Bytes !ByteString | Null
  deriving (Eq, Ord, Show)

foreign import ccall unsafe "sqlite3_open_v2"
  c_open :: CString -> Ptr (Ptr ()) -> CInt -> CString -> IO CInt
foreign import ccall unsafe "sqlite3_close_v2"
  c_close :: Ptr () -> IO CInt
foreign import ccall unsafe "sqlite3_errmsg"
  c_errmsg :: Ptr () -> IO CString
foreign import ccall unsafe "sqlite3_prepare_v2"
  c_prepare :: Ptr () -> CString -> CInt -> Ptr (Ptr ()) -> Ptr CString -> IO CInt
foreign import ccall unsafe "sqlite3_finalize"
  c_finalize :: Ptr () -> IO CInt
foreign import ccall unsafe "sqlite3_reset"
  c_reset :: Ptr () -> IO CInt
foreign import ccall unsafe "sqlite3_clear_bindings"
  c_clear_bindings :: Ptr () -> IO CInt
foreign import ccall unsafe "sqlite3_bind_null"
  c_bind_null :: Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "sqlite3_bind_int64"
  c_bind_int64 :: Ptr () -> CInt -> CLLong -> IO CInt
foreign import ccall unsafe "sqlite3_bind_text64"
  c_bind_text :: Ptr () -> CInt -> CString -> Word64 -> FunPtr (Ptr () -> IO ()) -> CUChar -> IO CInt
foreign import ccall unsafe "sqlite3_step"
  c_step :: Ptr () -> IO CInt
foreign import ccall unsafe "sqlite3_column_count"
  c_column_count :: Ptr () -> IO CInt
foreign import ccall unsafe "sqlite3_column_type"
  c_column_type :: Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "sqlite3_column_int64"
  c_column_int64 :: Ptr () -> CInt -> IO CLLong
foreign import ccall unsafe "sqlite3_column_blob"
  c_column_blob :: Ptr () -> CInt -> IO (Ptr CUChar)
foreign import ccall unsafe "sqlite3_column_bytes"
  c_column_bytes :: Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "sqlite3_busy_timeout"
  c_busy_timeout :: Ptr () -> CInt -> IO CInt

-- SQLITE_TRANSIENT makes SQLite copy bound byte strings before this returns.
transient :: FunPtr (Ptr () -> IO ())
transient = castPtrToFunPtr (intPtrToPtr (-1))

openReadOnly :: FilePath -> IO Database
openReadOnly path = do
  database <- mallocBytes 8
  result <- withCString path $ \name -> c_open name database 0x1 nullPtr
  handle <- peek database
  free database
  if result /= 0 || handle == nullPtr
    then do
      message <- if handle == nullPtr then pure "cannot open SQLite database" else databaseError (Database handle)
      _ <- if handle == nullPtr then pure 0 else c_close handle
      throwIO (userError message)
    else do
      check (Database handle) =<< c_busy_timeout handle 60000
      pure (Database handle)

closeDatabase :: Database -> IO ()
closeDatabase (Database handle) = do
  _ <- c_close handle
  pure ()

withReadOnly :: FilePath -> (Database -> IO a) -> IO a
withReadOnly path = bracket (openReadOnly path) closeDatabase

prepare :: Database -> ByteString -> IO Statement
prepare database@(Database handle) sql =
  bracketOnError (mallocBytes 8) free $ \statement -> do
    result <- B.useAsCStringLen sql $ \(text, len) ->
      c_prepare handle text (fromIntegral len) statement nullPtr
    pointer <- peek statement
    if result /= 0 || pointer == nullPtr
      then databaseError database >>= throwIO . userError
      else pure (Statement pointer)

closeStatement :: Statement -> IO ()
closeStatement (Statement statement) = do
  _ <- c_finalize statement
  pure ()

withStatement :: Database -> ByteString -> (Statement -> IO a) -> IO a
withStatement database sql = bracket (prepare database sql) closeStatement

bindValues :: Database -> Statement -> [Value] -> IO ()
bindValues database statement values = do
  check database =<< c_reset (statementPtr statement)
  check database =<< c_clear_bindings (statementPtr statement)
  mapM_ bind (zip [1..] values)
  where
    bind (index, value) = case value of
      Null -> check database =<< c_bind_null (statementPtr statement) index
      Integer number -> check database =<< c_bind_int64 (statementPtr statement) index (fromIntegral number)
      Bytes bytes -> B.useAsCStringLen bytes $ \(pointer, len) ->
        check database =<< c_bind_text (statementPtr statement) index pointer (fromIntegral len) transient 1

query :: Database -> Statement -> [Value] -> IO [[Value]]
query database statement values = do
  bindValues database statement values
  rows statement []
  where
    rows (Statement handle) acc = do
      result <- c_step handle
      case result of
        100 -> do
          count <- c_column_count handle
          row <- mapM (column handle) [0 .. count - 1]
          rows (Statement handle) (row:acc)
        101 -> pure (reverse acc)
        _ -> databaseError database >>= throwIO . userError

column :: Ptr () -> CInt -> IO Value
column statement index = do
  kind <- c_column_type statement index
  case kind of
    1 -> Integer . fromIntegral <$> c_column_int64 statement index
    5 -> pure Null
    _ -> do
      pointer <- c_column_blob statement index
      size <- c_column_bytes statement index
      if size < 0 || (size > 0 && pointer == nullPtr)
        then throwIO (userError "invalid SQLite column value")
        else Bytes <$> B.packCStringLen (castPtr pointer, fromIntegral size)

queryValues :: Database -> ByteString -> [Value] -> IO [[Value]]
queryValues database sql values = withStatement database sql $ \statement ->
  query database statement values

queryRows :: Database -> ByteString -> [Value] -> IO [[ByteString]]
queryRows database sql values = do
  rows <- queryValues database sql values
  pure (map (map bytes) rows)
  where
    bytes (Bytes value) = value
    bytes Null = B.empty
    bytes (Integer value) = error ("integer SQLite value where bytes expected: " ++ show value)

statementPtr :: Statement -> Ptr ()
statementPtr (Statement pointer) = pointer

check :: Database -> CInt -> IO ()
check database result
  | result == 0 = pure ()
  | otherwise = databaseError database >>= throwIO . userError

databaseError :: Database -> IO String
databaseError (Database handle) = do
  message <- c_errmsg handle
  if message == nullPtr then pure "unknown SQLite error" else peekCString message
