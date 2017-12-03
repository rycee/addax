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

{-# LANGUAGE DeriveDataTypeable #-}

module Addax.CmdLine
       ( addaxCmdArgs
       , Modes(..)
       ) where

import           Addax.About (summaryText)
import           Addax.Interval (Interval, readInterval)
import           Addax.Types (Feed)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Database.Persist (Key)
import           Database.Persist.Sql (toSqlKey)
import           Options.Applicative
import           Text.URI (URI, parseURI)

data Modes =
  About
  | Add { interval :: Maybe Interval, url :: URI }
  | Export { file :: FilePath }
  | Fetch
  | Import { file :: FilePath }
  | List
  | Remove { feedId :: Key Feed }
  | Ui
  deriving (Show)

optUi :: Mod CommandFields Modes
optUi = command "ui" (info opts mods)
  where
    opts = pure Ui
    mods = progDesc "Opens interactive user interface"

optFetch :: Mod CommandFields Modes
optFetch = command "fetch" (info opts mods)
  where
    opts = pure Fetch
    mods = progDesc "Updates subscribed feeds"

optAdd :: Mod CommandFields Modes
optAdd = command "add" (info opts mods)
  where
    opts = Add <$> optional intervalOpt
               <*> argument uriR (metavar "URL")
    mods = progDesc "Adds a new feed subscription"
    intervalR = eitherReader readInterval
    intervalOpt = option intervalR (long "interval" <> metavar "INTERVAL")
    uriR = eitherReader (maybe (Left "Invalid URL") Right . parseURI)

optRemove :: Mod CommandFields Modes
optRemove = command "remove" (info opts mods)
  where
    opts = Remove <$> argument keyR (metavar "FEED-ID")
    mods = progDesc "Removes a subscribed feed and all its items"
    keyR = toSqlKey <$> auto

optList :: Mod CommandFields Modes
optList = command "list" $
    info (pure List) (progDesc "Prints list of subscribed feeds")

optImport :: Mod CommandFields Modes
optImport = command "import" $ info opts mods
  where
    opts = Import <$> argument str (metavar "FILE")
    mods = progDesc "Imports an OPML file of feeds"

optExport :: Mod CommandFields Modes
optExport = command "export" $ info opts mods
  where
    opts = Export <$> argument str (metavar "FILE")
    mods = progDesc "Exports list subscribed feeds"

optAbout :: Mod CommandFields Modes
optAbout = command "about" (info opts mods)
  where
    opts = pure About
    mods = progDesc "Shows a description of this program"

cmds :: ParserInfo Modes
cmds = info (helper <*> opts) mods
  where
    opts = hsubparser
        $ optUi
        <> optFetch
        <> optAdd
        <> optRemove
        <> optList
        <> optImport
        <> optExport
        <> optAbout
    mods = progDesc (T.unpack summaryText)

addaxCmdArgs :: IO Modes
addaxCmdArgs = execParser cmds
