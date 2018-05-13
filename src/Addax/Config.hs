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
       , confFetchTimeout
       , loadConfig
       ) where

import           Addax.Interval
import           Control.Lens
import           Data.Ini.Config
import           Data.Monoid ((<>))
import qualified Data.Text.IO as T
import           System.Directory (doesFileExist)
import           System.Exit
import           System.IO

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

    -- | Length of the HTTP response timeout.
    , _confFetchTimeout :: !Interval
  }
  deriving (Eq, Show)

makeLenses ''AddaxConfig

configParser :: IniParser AddaxConfig
configParser =
  section "addax"
  $ AddaxConfig
      <$> (fieldDefOf "default-interval" readIntervalText (read "12h"))
      <*> (fieldDefOf "expire-after"     readIntervalText (read "4w"))
      <*> (fieldFlagDef "expire-unread" False)
      <*> (fieldDefOf "fetch-timeout"    readIntervalText (read "20s"))

-- | Loads an Addax configuration.  If no prior configuration file
-- could be found then returns `defaultConfig`.
loadConfig :: FilePath -> IO AddaxConfig
loadConfig cfgPath =
  do
    cfgExists <- doesFileExist cfgPath
    ini <- if cfgExists then T.readFile cfgPath else return defaultFile
    either handleError return (parseIniFile ini configParser)
  where
    defaultFile = "[addax]\n"

    handleError msg =
      do
        hPutStrLn stderr $ "Error reading config file " <> cfgPath <> ": " <> msg
        exitFailure
