{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Goonix.Build.Users
  ( BuildUser (..)
  , UserPool
  , parsePasswd
  , parseGroup
  , allocateUsers
  , acquireUser
  , releaseUser
  , createUserPool
  , withBuildUser
  , loadUserPool
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, retry, writeTVar)
import Control.Exception (bracket)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Goonix.Store.ReadOnly (storeError)
import Control.Exception (throwIO)
import System.Posix.Env.ByteString (getEnv)

loadUserPool :: B.ByteString -> IO (Maybe UserPool)
loadUserPool group
  | B.null group = pure Nothing
  | otherwise = do
      groupFile <- maybe "/etc/group" id <$> getEnv "GUIX_BUILD_GROUP_FILE"
      passwdFile <- maybe "/etc/passwd" id <$> getEnv "GUIX_BUILD_PASSWD_FILE"
      groups <- either (throwIO . storeError) pure . parseGroup group =<< B.readFile (C.unpack groupFile)
      users <- either (throwIO . storeError) pure . parsePasswd =<< B.readFile (C.unpack passwdFile)
      selected <- either (throwIO . storeError) pure (allocateUsers groups users)
      Just <$> createUserPool selected
data BuildUser = BuildUser
  { buildUserName :: B.ByteString
  , buildUserUid :: Word32
  , buildUserGid :: Word32
  } deriving (Eq, Show)

data UserPool = UserPool (TVar [BuildUser])

-- Parse the numeric fields only; comments and malformed entries are rejected.
parsePasswd :: B.ByteString -> Either String (Map.Map B.ByteString (Word32, Word32))
parsePasswd = fmap Map.fromList . mapM entry . filter (not . B.null) . C.lines
  where
    entry line = case C.split ':' line of
      (name:_:uid:gid:_) | not (B.null name) -> do
        u <- number uid
        g <- number gid
        pure (name, (u, g))
      _ -> Left "malformed passwd entry"
    number bytes = case reads (C.unpack bytes) of
      [(n, "")] | n >= 0 && n <= 4294967295 -> pure (fromIntegral n)
      _ -> Left "invalid numeric user or group id"

parseGroup :: B.ByteString -> B.ByteString -> Either String [B.ByteString]
parseGroup group contents = case filter matches (filter (not . B.null) (C.lines contents)) of
  [line] -> case C.split ':' line of
    [_, _, _, members] -> pure (filter (not . B.null) (C.split ',' members))
    _ -> Left "malformed build group entry"
  _ -> Left ("build group not found: " ++ C.unpack group)
  where
    matches line = case C.split ':' line of
      name:_ -> name == group
      _ -> False

allocateUsers :: [B.ByteString] -> Map.Map B.ByteString (Word32, Word32) -> Either String [BuildUser]
allocateUsers names users = mapM allocate (unique names)
  where
    unique = foldr (\name rest -> name : filter (/= name) rest) []
    allocate name = case Map.lookup name users of
      Just (uid, gid) | uid /= 0 && gid /= 0 -> Right (BuildUser name uid gid)
      _ -> Left ("unknown or unusable build user: " ++ C.unpack name)

createUserPool :: [BuildUser] -> IO UserPool
createUserPool = fmap UserPool . newTVarIO

acquireUser :: UserPool -> IO BuildUser
acquireUser (UserPool state) = atomically $ do
  available <- readTVar state
  case available of
    [] -> retry
    user:rest -> writeTVar state rest >> pure user

releaseUser :: UserPool -> BuildUser -> IO ()
releaseUser (UserPool state) user = atomically $ modifyTVar state (user :)

withBuildUser :: UserPool -> (BuildUser -> IO a) -> IO a
withBuildUser pool = bracket (acquireUser pool) (releaseUser pool)
