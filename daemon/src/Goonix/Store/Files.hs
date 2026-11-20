{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- Store filesystem operations use raw bytes, not locale-decoded Strings.
module Goonix.Store.Files
  ( restoreNar
  , canonicaliseTree
  , withPathLock
  , withGCLock
  , withTempRootFile
  , readTemporaryRootFile
  , withTemporaryRoots
  , readLock
  , appendTempRoot
  , deduplicateTree
  , makeSymlinkAtomic
  , mkdirParents
  , deleteTree
  , directoryEntries
  , catchMissing
  , writeBytes
  ) where

import Control.Exception (IOException, bracket, bracketOnError, finally, throwIO, try)
import Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar, modifyMVar_)
import qualified Data.Map.Strict as Map
import Control.Monad (foldM, forM_, unless, when)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.C.Error (Errno (..), eAGAIN, eNOENT, getErrno)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Data.Word (Word64)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Data.Bits ((.&.))
import Goonix.Store.Nar (Nar (..), hashNar)
import Goonix.Store.Base32 (encodeBase32)
import Goonix.Store.Hash (hashDigest)
import System.Posix.ByteString (RawFilePath)
import qualified System.Posix.Directory.ByteString as Dir
import qualified System.Posix.Files.ByteString as Files
import System.IO.Error (isDoesNotExistError)
import System.Posix.IO.ByteString (closeFd, openFd, OpenMode (WriteOnly), OpenFileFlags (..), defaultFileFlags, fdWrite)
import System.Posix.Types (Fd (..))

foreign import ccall unsafe "goonix_restore_node"
  c_restore_node :: CString -> CInt -> CInt -> Ptr () -> Word64 -> IO CInt
foreign import ccall unsafe "goonix_canonicalize" c_canonicalize :: CString -> IO CInt
foreign import ccall safe "goonix_lock_exclusive" c_lock_exclusive :: CString -> IO CInt
foreign import ccall unsafe "goonix_unlock_delete" c_unlock_delete :: CInt -> CString -> IO CInt
foreign import ccall unsafe "goonix_make_symlink" c_make_symlink :: CString -> CString -> IO CInt
foreign import ccall unsafe "goonix_rename" c_rename :: CString -> CString -> IO CInt
foreign import ccall unsafe "goonix_mkdir_parents" c_mkdir_parents :: CString -> CUInt -> IO CInt
foreign import ccall safe "goonix_lock_shared" c_lock_shared :: CString -> CInt -> IO CInt
foreign import ccall safe "goonix_lock_upgrade" c_lock_upgrade :: CInt -> CInt -> IO CInt
foreign import ccall safe "goonix_lock_nonblocking" c_lock_nonblocking :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "goonix_write_at_end" c_write_at_end :: CInt -> Ptr () -> Word64 -> IO CInt
foreign import ccall unsafe "goonix_dedup_link" c_dedup_link :: CString -> CString -> CString -> IO CInt

restoreNar :: RawFilePath -> Nar -> IO ()
restoreNar path = restore path
  where
    restore p (Regular executable contents) = withCStringBytes p $ \p' ->
      B.useAsCStringLen contents $ \(bytes, len) -> checkIO "cannot restore regular file" $
        c_restore_node p' 0 (if executable then 1 else 0) (castBytes bytes) (fromIntegral len)
    restore p (Symlink target) = withCStringBytes p $ \p' ->
      withCStringBytes target $ \target' -> checkIO "cannot restore symlink" $
        c_restore_node p' 1 0 (castBytes target') (fromIntegral (B.length target))
    restore p (Directory entries) = do
      withCStringBytes p $ \p' -> checkIO "cannot restore directory" $ c_restore_node p' 2 0 nullPtr 0
      forM_ entries $ \(name, child) -> restore (p <> "/" <> name) child

writeBytes :: RawFilePath -> ByteString -> IO ()
writeBytes path bytes = bracketOnError (openFd path WriteOnly defaultFileFlags { System.Posix.IO.ByteString.creat = Just 0o666 }) closeFd $ \fd -> do
  let writeAll rest
        | B.null rest = pure ()
        | otherwise = do
            written <- fdWrite fd rest
            when (written == 0) (ioError (userError "cannot write file"))
            writeAll (B.drop (fromIntegral written) rest)
  writeAll bytes

canonicaliseTree :: RawFilePath -> IO ()
canonicaliseTree path = do
  status <- Files.getSymbolicLinkStatus path
  withCStringBytes path (checkIO "cannot canonicalise store item" . c_canonicalize)
  when (Files.isDirectory status) $ do
    entries <- directoryEntries path
    forM_ entries $ \entry -> canonicaliseTree (path <> "/" <> entry)

withPathLock :: RawFilePath -> IO a -> IO a
withPathLock path action = do
  let directory = parent path
  status <- Files.getSymbolicLinkStatus directory
  let mode = Files.fileMode status
  Files.setFileMode directory 0o755
  bracket acquire release (const action) `finally` Files.setFileMode directory mode
  where
    lockPath = path <> ".lock"
    acquire = do
      fd <- withCStringBytes lockPath c_lock_exclusive
      if fd < 0 then errnoIO "cannot acquire store path lock" else pure (Fd fd)
    release (Fd fd) = do
      result <- withCStringBytes lockPath (c_unlock_delete fd)
      when (result < 0) (errnoIO "cannot release store path lock")

withGCLock :: RawFilePath -> IO a -> IO a
withGCLock = gcLock True

withGCReadLock :: RawFilePath -> IO a -> IO a
withGCReadLock = gcLock False

gcLock :: Bool -> RawFilePath -> IO a -> IO a
gcLock exclusive path action = do
  mkdirParents (parent path)
  bracket acquire closeFd (const action)
  where
    acquire = do
      fd <- withCStringBytes path (\p -> c_lock_shared p 1)
      if fd < 0 then errnoIO "cannot acquire global GC lock"
        else do
          when exclusive (checkIO "cannot upgrade global GC lock" (c_lock_upgrade fd 1))
          pure (Fd fd)

withTempRootFile :: RawFilePath -> RawFilePath -> (Fd -> IO a) -> IO a
withTempRootFile path gcPath action = do
  mkdirParents (parent path)
  bracket acquire release (action . rootDescriptor)
  where
    acquire = withMVar tempRootLock $ \() -> modifyMVar tempRoot $ \roots ->
      case Map.lookup path roots of
        Just root -> pure (Map.insert path root { rootUsers = rootUsers root + 1 } roots, root)
        Nothing -> withGCReadLock gcPath $ do
          root <- openRoot
          pure (Map.insert path root roots, root)
    openRoot = do
      fd <- withCStringBytes path c_lock_exclusive
      if fd >= 0 then do
        let descriptor = Fd fd
        readLock descriptor
        pure (Root descriptor [] 1)
      else do
        Errno errno <- getErrno
        if Errno errno == eAGAIN then Files.removeLink path `catchMissing` pure () >> openRoot
          else errnoIO "cannot acquire temporary roots lock"
    release root = withMVar tempRootLock $ \() -> modifyMVar_ tempRoot $ \roots ->
      case Map.lookup path roots of
        Nothing -> throwIO (userError "temporary root file released without an owner")
        Just current
          | rootUsers current > 1 -> pure (Map.insert path current { rootUsers = rootUsers current - 1 } roots)
          | otherwise -> do
              closeFd (rootDescriptor current)
              Files.removeLink path `catchMissing` pure ()
              pure (Map.delete path roots)

data Root = Root
  { rootDescriptor :: !Fd
  , rootPaths :: ![ByteString]
  , rootUsers :: !Int
  }

{-# NOINLINE tempRoot #-}
tempRoot :: MVar (Map.Map RawFilePath Root)
tempRoot = unsafePerformIO (newMVar Map.empty)

{-# NOINLINE tempRootLock #-}
tempRootLock :: MVar ()
tempRootLock = unsafePerformIO (newMVar ())

withTemporaryRoots :: RawFilePath -> ([ByteString] -> IO a) -> IO a
withTemporaryRoots directory action = withMVar tempRootLock $ \() -> do
  own <- withMVar tempRoot $ pure . Map.filterWithKey (\path _ -> (directory <> "/") `B.isPrefixOf` path)
  bracket (readRoots [] own) (mapM_ closeFd . snd) (action . fst)
  where
    readRoots locks own = do
      entries <- directoryEntries directory
      let external = [file | file <- entries, not (Map.member (directory <> "/" <> file) own)]
      foldM readOne (concatMap rootPaths (Map.elems own), locks) external
    readOne (roots, locks) file = do
      let path = directory <> "/" <> file
      fd <- withCStringBytes path (\p -> c_lock_shared p 0)
      if fd < 0 then do
        Errno errno <- getErrno
        if Errno errno == eNOENT then pure (roots, locks)
          else ioError (userError "cannot open temporary roots file")
      else do
        result <- c_lock_nonblocking fd 1
        if result == 0 then do
          Files.removeLink path `catchMissing` pure ()
          _ <- fdWrite (Fd fd) "d"
          closeFd (Fd fd)
          pure (roots, locks)
        else do
          readLock (Fd fd)
          temporary <- readTemporaryRootFile (Fd fd)
          pure (roots ++ temporary, Fd fd : locks)

readTemporaryRootFile :: Fd -> IO [ByteString]
readTemporaryRootFile (Fd fd) = filter (not . B.null) . B.split 0 . B.concat . reverse <$> readChunks 0 []
  where
    readChunks offset acc = do
      (buffer, count) <- allocaBytes 65536 $ \buffer -> do
        count <- c_read fd buffer 65536 offset
        pure (buffer, count)
      if count < 0 then errnoIO "cannot read temporary roots file"
        else if count == 0 then pure acc
          else do
            bytes <- B.packCStringLen (castPtr buffer, count)
            readChunks (offset + fromIntegral count) (bytes : acc)

appendTempRoot :: Fd -> RawFilePath -> IO ()
readLock :: Fd -> IO ()
readLock (Fd fd) = checkIO "cannot acquire temporary roots read lock" (c_lock_upgrade fd 0)

appendTempRoot fd path = withMVar tempRootLock $ \() -> do
  found <- modifyMVar tempRoot $ \roots ->
    case [(name, root) | (name, root) <- Map.toList roots, rootDescriptor root == fd] of
      [] -> pure (roots, False)
      (name, root):_ -> pure (Map.insert name root { rootPaths = path : rootPaths root } roots, True)
  unless found $ throwIO (userError "cannot append to an unowned temporary roots file")
  checkIO "cannot upgrade temporary roots lock" (c_lock_upgrade (case fd of Fd raw -> raw) 1)
  result <- B.useAsCStringLen (path <> "\0") $ \(bytes, len) -> c_write_at_end (case fd of Fd raw -> raw) (castPtr bytes) (fromIntegral len)
  when (result < 0) (errnoIO "cannot write temporary root")
  checkIO "cannot downgrade temporary roots lock" (c_lock_upgrade (case fd of Fd raw -> raw) 0)

foreign import ccall safe "pread"
  c_read :: CInt -> Ptr () -> Word64 -> Word64 -> IO Int

deduplicateTree :: RawFilePath -> RawFilePath -> IO ()
deduplicateTree store path = do
  unless (store == path) $ do
    mkdirParents (store <> "/.links")
    walk path
  where
    walk p = do
      status <- Files.getSymbolicLinkStatus p
      if Files.isDirectory status
        then directoryEntries p >>= mapM_ (\entry -> walk (p <> "/" <> entry))
        else when (Files.isRegularFile status && Files.fileSize status >= 8192 && Files.fileMode status .&. 0o200 == 0) $ do
          digest <- hashFileNar p
          let linkPath = store <> "/.links/" <> digest
              temporary = store <> "/.tmp-goonix-dedup"
          result <- withCStringBytes p $ \p' -> withCStringBytes linkPath $ \link' -> withCStringBytes temporary $ \temp' -> c_dedup_link p' link' temp'
          when (result < 0) (errnoIO "cannot deduplicate store file")
    hashFileNar p = do
      hash <- hashNar p
      pure (encodeBase32 (hashDigest hash))

makeSymlinkAtomic :: RawFilePath -> RawFilePath -> IO ()
makeSymlinkAtomic target link = do
  mkdirParents (parent link)
  let temporary = link <> ".tmp-goonix"
  Files.removeLink temporary `catchMissing` pure ()
  withCStringBytes target $ \target' -> withCStringBytes temporary $ \temporary' ->
    checkIO "cannot create symbolic link" (c_make_symlink target' temporary')
  withCStringBytes temporary $ \from -> withCStringBytes link $ \to ->
    checkIO "cannot rename symbolic link" (c_rename from to)

mkdirParents :: RawFilePath -> IO ()
mkdirParents path = withCStringBytes path $ \path' -> checkIO "cannot create directories" (c_mkdir_parents path' 0o755)

deleteTree :: RawFilePath -> IO ()
deleteTree path = do
  -- Use lstat, not fileExist: a symlink to another store path is dangling
  -- outside the private sandbox root, but its directory entry still needs
  -- to be removed.
  result <- try (Files.getSymbolicLinkStatus path)
  case result of
    Left error' | isDoesNotExistError error' -> pure ()
                | otherwise -> throwIO (error' :: IOException)
    Right status -> remove status
  where
    remove status
      | Files.isDirectory status = do
          Files.setFileMode path 0o755
          entries <- directoryEntries path
          forM_ entries $ \entry -> deleteTree (path <> "/" <> entry)
          Dir.removeDirectory path
      | otherwise = do
          let directory = parent path
          parentStatus <- Files.getSymbolicLinkStatus directory
          Files.setFileMode directory 0o755
          (unless (Files.isSymbolicLink status) (Files.setFileMode path 0o644) >> Files.removeLink path)
            `finally` Files.setFileMode directory (Files.fileMode parentStatus)

directoryEntries :: RawFilePath -> IO [RawFilePath]
directoryEntries path = bracket (Dir.openDirStream path) Dir.closeDirStream $ \stream -> do
  let loop acc = do
        entry <- Dir.readDirStream stream
        if B.null entry then pure (reverse acc)
          else if entry == "." || entry == ".." then loop acc else loop (entry:acc)
  loop []

parent :: RawFilePath -> RawFilePath
parent path = case B.breakEnd (== 47) path of
  ("", _) -> "."
  (directory, _) -> if directory == "/" then "/" else B.init directory

catchMissing :: IO a -> IO a -> IO a
catchMissing action fallback = do
  result <- try action
  case result of
    Right value -> pure value
    Left error' | isDoesNotExistError error' -> fallback
                | otherwise -> throwIO (error' :: IOException)

withCStringBytes :: RawFilePath -> (CString -> IO a) -> IO a
withCStringBytes = B.useAsCString

castBytes :: Ptr a -> Ptr ()
castBytes = castPtr

checkIO :: String -> IO CInt -> IO ()
checkIO message action = do
  result <- action
  when (result < 0) (errnoIO message)

errnoIO :: String -> IO a
errnoIO message = do
  Errno number <- getErrno
  ioError (userError (message ++ ": errno=" ++ show number))
