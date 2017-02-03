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

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Addax.Types where

import           Addax.Interval (Interval(..))
import           Addax.PersistFields ()
import           Control.Lens
import           Control.Monad (forM)
import           Control.Monad.Logger (LoggingT, logInfoN)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime(..), getCurrentTime, addUTCTime, fromGregorian)
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH
import           Text.URI (URI, nullURI)

share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"
      ] [persistLowerCase|

Feed
  url URI
  title Text
  updateTitle Bool
  website URI Maybe
  addedAt UTCTime
  updatedAt UTCTime Maybe
  updateInterval Interval Maybe
  expireInterval Interval Maybe
  expireUnread Bool Maybe
  autoUpdate Bool
  UniqueFeedUrl url
  deriving Show

FeedItem
  feed FeedId
  url URI
  title Text
  author Text Maybe
  body Text
  publishedAt UTCTime Maybe
  downloadedAt UTCTime
  isRead Bool
  isPinned Bool
  UniqueFeedItemUrl url
  deriving Show

FeedTag
  feed FeedId
  tag Text

|]

-- | Given a default interval and a feed, return the feed update
-- interval.
feedIntervalWithDefault :: Interval -> Feed -> Interval
feedIntervalWithDefault defaultInterval feed =
    maybe defaultInterval id (feed ^. feedUpdateInterval)

-- | Whether a given feed is due for update for the given time.
feedUpdateIsDue :: Interval -> UTCTime -> Feed -> Bool
feedUpdateIsDue defaultInterval now feed =
    now >= feedNextUpdateAt defaultInterval feed

-- | Given a default interval and a feed, return the next update time.
--
-- If the feed has not been updated before, returns the Unix epoch
-- time 0.
feedNextUpdateAt :: Interval -> Feed -> UTCTime
feedNextUpdateAt defaultInterval feed =
  case feed ^. feedUpdatedAt of
    Nothing -> UTCTime (fromGregorian 1970 1 1) 0
    Just updatedAt ->
      let Interval feedInterval = feedIntervalWithDefault defaultInterval feed
      in  addUTCTime (realToFrac feedInterval) updatedAt

-- | Finds all feeds that are due for update.
feedsToUpdate :: MonadIO m => Interval -> Bool -> SqlPersistT m [Entity Feed]
feedsToUpdate defInterval onlyAuto =
  do
    now <- liftIO getCurrentTime
    filterDue now <$> selectList cond []
  where
    cond = if onlyAuto then [ FeedAutoUpdate ==. True ] else [ ]
    filterDue now = filter (feedUpdateIsDue defInterval now . entityVal)

-- | An empty feed value.
emptyFeed :: Feed
emptyFeed =
    Feed { _feedUrl = nullURI
         , _feedTitle = ""
         , _feedUpdateTitle = True
         , _feedWebsite = Nothing
         , _feedAddedAt = UTCTime (fromGregorian 0 0 0) 0
         , _feedUpdatedAt = Nothing
         , _feedUpdateInterval = Nothing
         , _feedExpireInterval = Nothing
         , _feedExpireUnread = Nothing
         , _feedAutoUpdate = True
         }

-- | Adds a new feed with the given interval and URL. If a title is
-- given then the title is not updated from the feed.
addFeed :: MonadIO m => Maybe Interval -> Maybe Text -> URI -> SqlPersistT (LoggingT m) ()
addFeed interval title url =
  do
    now <- liftIO getCurrentTime
    key <- insert
        . set feedUrl url
        . set feedTitle (maybe "" id title)
        . set feedUpdateTitle (isJust title)
        . set feedAddedAt now
        . set feedUpdateInterval interval
        $ emptyFeed
    logInfoN ("Added feed with key " <> T.pack (show key))
    return ()

removeFeed :: MonadIO m => Key Feed -> SqlPersistT (LoggingT m) ()
removeFeed key =
  do
    logInfoN $ "Deleting feed with key " <> T.pack (show key) <> " and all its items"
    deleteCascade key
    logInfoN $ "Completed delete of feed " <> T.pack (show key)

getFeedWithUnreadCount :: MonadIO m => SqlPersistT m [(Entity Feed, Int)]
getFeedWithUnreadCount =
  do
    results <- rawSql sql []
    forM results $ \ (feed, Single numUnread) ->
      return (feed, numUnread)
  where
    sql =
      "select ??, count(feed_item.is_read) "
      <> "from feed "
      <> "left join feed_item on (feed_item.feed = feed.id and feed_item.is_read = 0) "
      <> "group by feed.id "
      <> "order by feed.title asc"

updateItemRead :: MonadIO m => Bool -> FeedItem -> SqlPersistT m ()
updateItemRead isRead feedItem =
    updateWhere
        [ FeedItemUrl ==. feedItem ^. feedItemUrl ]
        [ FeedItemIsRead =. isRead ]
