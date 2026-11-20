{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Goonix.Build.Users
import System.Exit (die)
import System.Timeout (timeout)

check :: String -> Bool -> IO ()
check label condition = unless condition (die ("FAIL: " ++ label))

main :: IO ()
main = do
  let passwd = "root:x:0:0:root:/:/noshell\nbuilder-a:x:30001:30000:a:/:/noshell\nbuilder-b:x:30002:30000:b:/:/noshell\n"
      groups = "root:x:0:\nguixbuild:x:30000:builder-a,builder-b,builder-a\n"
  users <- either die pure (parsePasswd passwd)
  check "passwd numeric identities" (Map.lookup "builder-b" users == Just (30002, 30000))
  names <- either die pure (parseGroup "guixbuild" groups)
  check "group selection and duplicate removal" (names == ["builder-a", "builder-b", "builder-a"])
  selected <- either die pure (allocateUsers names users)
  check "duplicate build users allocated once" (selected == [BuildUser "builder-a" 30001 30000, BuildUser "builder-b" 30002 30000])
  check "missing group rejected" (parseGroup "missing" groups == Left "build group not found: missing")
  check "root passwd entry rejected" (case allocateUsers ["root"] users of Left _ -> True; _ -> False)
  check "unknown build user rejected" (case allocateUsers ["absent"] users of Left _ -> True; _ -> False)

  pool <- createUserPool selected
  first <- acquireUser pool
  second <- acquireUser pool
  check "distinct concurrent identities" (first /= second)
  blocked <- timeout 100000 (acquireUser pool)
  check "exhausted pool waits" (blocked == Nothing)
  releaseUser pool first
  reused <- timeout 1000000 (acquireUser pool)
  check "release makes identity available" (reused == Just first)
  releaseUser pool second
  releaseUser pool first

  result <- try (withBuildUser pool $ \_ -> error "builder failed") :: IO (Either SomeException ())
  check "builder exception propagated" (case result of Left _ -> True; _ -> False)
  after <- timeout 1000000 (acquireUser pool)
  check "identity released after exception" (after /= Nothing)
  putStrLn "HS-6 build-user parsing and allocation: PASS"
