-- Copyright (C) 2017 Robert Helgesson <robert@rycee.net>
--
-- Addax is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Addax is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Addax.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Addax.Config
       ( AddaxConfig
       , confDefaultInterval
       , confExpireInterval
       , confExpireUnread
       , confOpenCommand
       , confFetchTimeout
       , loadConfig
       ) where

import Addax.Interval
import Control.Lens
import Data.Configurator
import Data.Text (Text)

data AddaxConfig =
  AddaxConfig {
    -- | The default update interval to use for new feed
    -- subscriptions.
    _confDefaultInterval :: !Interval

    -- | How long to keep items before deleting them from the
    -- database.
    , _confExpireInterval :: !Interval

    -- | Whether to expire unread items.
    , _confExpireUnread :: !Bool

    -- | Command to use when opening links, default is @xdg-open@.
    , _confOpenCommand :: !Text

    -- | Length of the HTTP response timeout.
    , _confFetchTimeout :: !Interval
  }
  deriving (Eq, Show)

makeLenses ''AddaxConfig

-- -- | Produces a string representation of the given configuration.
-- configToString :: Config -> Either CPError String
-- configToString Config {..} =
--   do let cp = emptyCP
--      cp <- add_section cp "config"
--      cp <- set cp "config" "default-interval" (show defaultInterval)
--      cp <- set cp "config" "expire-interval" (show expireInterval)
--      cp <- set cp "config" "expire-unread" (show expireUnread)
--      return . to_string $ cp

-- | Loads an Addax configuration.  If no prior configuration file
-- could be found then returns `defaultConfig`.
loadConfig :: FilePath -> IO AddaxConfig
loadConfig cfgPath =
  do
    cfg <- load [Optional cfgPath]
    AddaxConfig
      <$> (read <$> lookupDefault "12h" cfg "default-interval")
      <*> (read <$> lookupDefault "4w" cfg "expire-after")
      <*> lookupDefault False cfg "expire-unread"
      <*> lookupDefault "xdg-open" cfg "open-command"
      <*> (read <$> lookupDefault "20s" cfg "fetch-timeout")

-- -- | Saves an Addax configuration into the standard user configuration
-- -- file.  Note, if the given configuration is the same as the
-- -- `defaultConfig` then no file is written.
-- saveConfig :: Config -> IO ()
-- saveConfig config | config == defaultConfig = return ()
--                   | otherwise =
--   do file <- configFile
--      case configToString config of
--        Left (err, loc) -> hPutStrLn stderr $ loc ++ ": " ++ show err
--        Right str -> writeFile file str
