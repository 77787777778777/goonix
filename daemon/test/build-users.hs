{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Goonix.Build.Users
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (die)
import System.Timeout (timeout)

check :: String -> Bool -> IO ()
check label condition = unless condition (die ("FAIL: " ++ label))

main :: IO ()
main = do
  let passwd = "root:x:0:0:root:/:/noshell\n# ignored\nbuilder-a:x:30001:39999:a:/:/noshell\nbuilder-b:x:30002:39999:b:/:/noshell\n"
      groups = "root:x:0:\n# ignored\nguixbuild:x:30000:builder-a,builder-b,builder-a\nkvm:x:31000:builder-a\n"
      users = parsePasswd passwd
  check "passwd numeric identities" (Map.lookup "builder-b" users == Just (30002, 39999))
  (groupGid, names) <- either die pure (parseGroup "guixbuild" groups)
  check "group GID and members" (groupGid == 30000 && names == ["builder-a", "builder-b", "builder-a"])
  selected <- either die pure (allocateUsers groupGid names users)
  check "group GID overrides primary group" (all ((== 30000) . buildUserGid) selected)
  check "duplicate build users allocated once" (length selected == 2)
  check "missing group rejected" (parseGroup "missing" groups == Left "build group not found: missing")
  check "root passwd entry rejected" (case allocateUsers 30000 ["root"] users of Left _ -> True; _ -> False)
  check "unknown build user rejected" (case allocateUsers 30000 ["absent"] users of Left _ -> True; _ -> False)

  tmp <- getTemporaryDirectory
  let state = tmp ++ "/goonix-build-users-test"
  createDirectoryIfMissing True state
  pool <- createUserPool (C.pack state) selected
  first <- acquireUser pool
  second <- acquireUser pool
  check "distinct concurrent identities" (buildUserUid (userLease first) /= buildUserUid (userLease second))
  blocked <- try (acquireUser pool) :: IO (Either SomeException UserLease)
  check "exhausted pool reports an error" (case blocked of Left _ -> True; Right _ -> False)
  releaseLease pool first
  reused <- timeout 1000000 (acquireUser pool)
  check "release makes identity available" (maybe False ((== buildUserUid (userLease first)) . buildUserUid . userLease) reused)
  releaseLease pool second
  case reused of
    Just lease -> releaseLease pool lease
    Nothing -> pure ()
  -- A second pool in the same process sees the first pool's table entry;
  -- the fcntl lock also excludes another daemon process.
  held <- acquireUser pool
  other <- createUserPool (C.pack state) selected
  nextResult <- newEmptyMVar
  _ <- forkIO $ acquireUser other >>= putMVar nextResult
  next <- timeout 1000000 (takeMVar nextResult)
  check "second pool skips held UID" (maybe False ((/= buildUserUid (userLease held)) . buildUserUid . userLease) next)
  case next of Just lease -> releaseLease other lease; Nothing -> pure ()
  releaseLease pool held
  result <- try (withBuildUser pool $ \_ -> error "builder failed") :: IO (Either SomeException ())
  check "builder exception propagated" (case result of Left _ -> True; _ -> False)
  after <- timeout 1000000 (acquireUser pool)
  check "identity released after exception" (case after of Just _ -> True; Nothing -> False)
  removeDirectoryRecursive state
  putStrLn "HS-6 build-user parsing and allocation: PASS"
