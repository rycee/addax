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

module Addax.About
       ( copyrightText
       , summaryText
       , aboutText
       ) where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Paths_addax (version)

versionText :: Text
versionText = "v" <> T.pack (showVersion version)

copyrightText :: Text
copyrightText = "Copyright © 2017 Robert Helgesson"

summaryText :: Text
summaryText = "Addax " <> versionText <> ", " <> copyrightText

aboutText :: Text
aboutText =
    T.unlines [ ""
              , "    ▄▄           ▄▄        ▄▄                     "
              , "   ████          ██        ██                     "
              , "   ████     ▄███▄██   ▄███▄██   ▄█████▄  ▀██  ██▀ "
              , "  ██  ██   ██▀  ▀██  ██▀  ▀██   ▀ ▄▄▄██    ████   "
              , "  ██████   ██    ██  ██    ██  ▄██▀▀▀██    ▄██▄   "
              , " ▄██  ██▄  ▀██▄▄███  ▀██▄▄███  ██▄▄▄███   ▄█▀▀█▄  "
              , " ▀▀    ▀▀    ▀▀▀ ▀▀    ▀▀▀ ▀▀   ▀▀▀▀ ▀▀  ▀▀▀  ▀▀▀ "
              , "                                    " <> versionText
              , ""
              , " The Addax feed reader."
              , ""
              , " Author"
              , ""
              , "   Robert Helgesson <robert@rycee.net>"
              , ""
              , " Copyright"
              , ""
              , "   " <> copyrightText
              , ""
              , "   Addax is free software: you can redistribute it and/or modify it"
              , "   under the terms of the GNU General Public License as published by"
              , "   the Free Software Foundation, either version 3 of the License, or"
              , "   (at your option) any later version."
              , ""
              , "   Addax is distributed in the hope that it will be useful, but"
              , "   WITHOUT ANY WARRANTY; without even the implied warranty of"
              , "   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"
              , "   General Public License for more details."
              , ""
              , "   You should have received a copy of the GNU General Public License"
              , "   along with Addax.  If not, see <http://www.gnu.org/licenses/>."
              ]
