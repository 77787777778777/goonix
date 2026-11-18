{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Goonix.Store.Config
  ( ServerConfig (..)
  , defaultConfig
  ) where
import Data.ByteString (ByteString)
import Goonix.Store.Settings (Settings, defaultSettings)

data ServerConfig = ServerConfig
  { configStore :: !ByteString
  , configDatabase :: !FilePath
  , configDefaultSettings :: !Settings
  , configLogConnections :: !Bool
  , configReadOnly :: !Bool
  , configStateDirectory :: !(Maybe ByteString)
  , configGuixProgram :: !ByteString
  }

defaultConfig :: ServerConfig
defaultConfig = ServerConfig "/gnu/store" "/var/guix/db/db.sqlite"
  (defaultSettings True False) False True Nothing "guix"
