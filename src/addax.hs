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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Addax.About (aboutText)
import           Addax.AsyncUtil (withAsyncLog)
import qualified Addax.CmdLine as O
import           Addax.Config
import           Addax.Fetcher (UpdateEvent(..), updateFeeds)
import           Addax.Types
import           Addax.Ui
import           Control.Concurrent.Chan
import           Control.Lens
import           Control.Monad (forever, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runStderrLoggingT, filterLogger, LogLevel(LevelInfo), LoggingT, logDebugN, logInfoN, logErrorN)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import           Database.Persist.Sqlite (withSqliteConn)
import           System.Directory (createDirectoryIfMissing)
import           System.Environment.XDG.BaseDir (getUserConfigDir, getUserDataDir)
import           System.FilePath ((</>))

filterLogLevel :: LoggingT m a -> LoggingT m a
filterLogLevel = filterLogger (\ _ lvl -> lvl >= LevelInfo)

main :: IO ()
main = runStderrLoggingT $ filterLogLevel $
  do
    confDir <- liftIO $ getUserConfigDir "addax"
    let cfgFile = confDir </> "addax.conf"

    logDebugN $ "Configuration file is " <> T.pack cfgFile

    cmd <- liftIO $ O.addaxCmdArgs
    cfg <- liftIO $ loadConfig cfgFile

    runCmd cfg cmd

withDb :: (P.SqlBackend -> LoggingT IO a) -> LoggingT IO a
withDb run =
  do
    dataDir <- liftIO $ getUserDataDir "addax"
    let dbFile = T.pack $ dataDir </> "feeds.sqlite"
    logDebugN $ "Data file is " <> dbFile

    liftIO $ createDirectoryIfMissing True dataDir

    withSqliteConn (dbFile) $ \ sqlBackend ->
      do
        P.runSqlConn prepareDb sqlBackend
        run sqlBackend

prepareDb :: ReaderT P.SqlBackend (LoggingT IO) ()
prepareDb =
  do
    -- Do an automatic migration.
    P.runMigration migrateAll

    -- We often want to look up only unread items and an index is
    -- needed to make that fast.
    P.rawExecute "create index if not exists idx_feed_item_is_read \
                 \on feed_item (is_read)" []

runCmd :: AddaxConfig -> O.Modes -> LoggingT IO ()
runCmd _ O.About = liftIO . T.putStrLn $ aboutText
runCmd cfg O.Ui = withDb (runAddaxUi cfg)
runCmd cfg O.Fetch = withDb $ \sqlBackend ->
  do
    chan <- liftIO newChan
    withAsyncLog (reportDownloads chan) $ \ _ ->
        P.runSqlConn (updateFeeds defInterval chan) sqlBackend
  where
    defInterval = cfg ^. confDefaultInterval
runCmd _ (O.Import {..}) = return ()
runCmd _ (O.Export {..}) = return ()
runCmd _ (O.Add {..}) =
    withDb $ P.runSqlConn (addFeed interval Nothing url)
runCmd _ (O.Remove {..}) =
    withDb $ P.runSqlConn (removeFeed feedId)
runCmd cfg O.List = withDb $ P.runSqlConn go
  where
    go =
      do
        feeds <- P.selectList [] []
        forM_ feeds $ \ (P.Entity key feed) ->
          liftIO
              $ putStrLn
              $ show (P.fromSqlKey key)
              <> " - "
              <> T.unpack (feed ^. feedTitle)
              <> " - "
              <> show (feed ^. feedUrl)
              <> " - "
              <> show (feedNextUpdateAt (cfg ^. confDefaultInterval) feed)

-- | Continuously reports the update status taken from the given
-- channel.
reportDownloads :: Chan UpdateEvent -> LoggingT IO a
reportDownloads chan = forever (liftIO (readChan chan) >>= report)
  where
    report (UpdateError msg) = logErrorN msg
    report (Updating feed) =
      let name = if feed ^. feedTitle == ""
                 then T.pack (show (feed ^. feedUrl))
                 else feed ^. feedTitle
      in  logInfoN $ "Updating feed " <> name
    report (Updated feed items) =
      do
        let name = if feed ^. feedTitle == ""
                   then T.pack (show (feed ^. feedUrl))
                   else feed ^. feedTitle
            itemsTxt = T.concat . map (\i -> "\n  - " <> i ^. feedItemTitle) $ items
            msg = "Updated feed " <> name <> itemsTxt
        logInfoN msg
