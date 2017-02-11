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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Addax.Fetcher
       ( UpdateEvent(..)
       , updateFeeds
       ) where

import           Addax.AsyncUtil
import           Addax.Interval (Interval(..))
import           Addax.PersistFields ()
import           Addax.Types
import           Control.Concurrent.Chan
import           Control.Exception (catch)
import           Control.Lens
import           Control.Monad (join, forM)
import           Control.Monad.Logger (MonadLoggerIO, logDebugN, logErrorN)
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T hiding (replace)
import           Data.Time (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import qualified Network.HTTP.Types.Status as H
import qualified Text.Atom.Feed as Atom
import qualified Text.Feed.Import as F
import qualified Text.Feed.Query as F
import qualified Text.Feed.Types as F
import           Text.URI (parseURI)
import           Text.XML.Light.Proc (strContent)

data UpdateEvent = Updating Feed | Updated Feed [FeedItem] | UpdateError Text
  deriving (Show)

-- | Concurrently update all feeds that are due. Each newly updated
-- feed is placed on the given channel.
updateFeeds :: MonadLoggerIO m => Interval -> Chan UpdateEvent -> SqlPersistT m ()
updateFeeds defInterval eventChan =
  do
    toUpdate <- feedsToUpdate defInterval True
    fetchChan <- liftIO $ newChan
    _ <- asyncLog $ fetchAll fetchChan toUpdate
    updater fetchChan
  where
    updater fetchChan =
      do
        mres <- liftIO $ readChan fetchChan
        case mres of
          Nothing -> return ()
          Just res -> updateFeed eventChan res >> updater fetchChan
    fetchAll fetchChan toUpdate =
      do
        httpManager <- liftIO $ H.newManager H.tlsManagerSettings
        _ <- mapConcurrentlyLog (fetch fetchChan httpManager) toUpdate
        liftIO $ writeChan fetchChan Nothing
    fetch fetchChan httpManager feed =
      do
        liftIO $ writeChan eventChan $ Updating (entityVal feed)
        fetched <- fetchFeed httpManager feed
        liftIO $ writeChan fetchChan (Just fetched)
        return ()

updateFeed :: MonadLoggerIO m
           => Chan UpdateEvent
           -> Either Text (Entity Feed, F.Feed)
           -> SqlPersistT m ()
updateFeed chan = go
  where
    go (Left err) = liftIO $ writeChan chan $ UpdateError $ "Unable to update feed: " <> err
    go (Right (Entity feedId feed, rawFeed)) =
      do
        items <- forM (F.getFeedItems rawFeed) (updateOrInsertItem feedId)
        let title = T.strip . T.pack . F.getFeedTitle $ rawFeed
            website = parseURI =<< F.getFeedHTML rawFeed
        now <- liftIO getCurrentTime
        feed' <- updateGet feedId $
            [ FeedWebsite =. website
            , FeedUpdatedAt =. Just now
            ] ++ [ FeedTitle =. title | feed ^. feedUpdateTitle ]
        liftIO $ writeChan chan $ Updated feed' (catMaybes items)

    updateOrInsertItem feedId item =
      do
        now <- liftIO getCurrentTime
        case buildFeedItem now feedId item of
          Left err ->
            do
              logErrorN $ "Invalid feed item: " <> err
              return Nothing
          Right feedItem ->
            do
              mfeedItem <- selectFirst [ FeedItemUrl ==. feedItem ^. feedItemUrl ] []
              maybe (insertItem feedItem) (updateItem feedItem) mfeedItem

    itemsSame oldItem newItem =
        oldItem ^. feedItemFeed == newItem ^. feedItemFeed
        && oldItem ^. feedItemBody == newItem ^. feedItemBody
        && oldItem ^. feedItemPublishedAt == newItem ^. feedItemPublishedAt

    updateItem feedItem (Entity oldFeedItemId oldFeedItem)
      | itemsSame oldFeedItem feedItem =
          logDebugN "Reusing old feed item" >> return Nothing
      | otherwise = replace oldFeedItemId feedItem >> return (Just feedItem)

    insertItem feedItem = insert feedItem >> return (Just feedItem)

    buildFeedItem now feedId item =
      do
        url <- m2e "missing link" $ parseURI =<< F.getItemLink item
        title <- m2e "missing title" $ F.getItemTitle item
        let author = (T.strip . T.pack) <$> F.getItemAuthor item
            body = maybe "" id (getItemBody item)
            publishedAt = join (F.getItemPublishDate item)
        return FeedItem { _feedItemFeed = feedId
                        , _feedItemUrl = url
                        , _feedItemTitle = unescapeXml . T.strip . T.pack $ title
                        , _feedItemAuthor = author
                        , _feedItemBody = body
                        , _feedItemPublishedAt = publishedAt
                        , _feedItemDownloadedAt = now
                        , _feedItemIsRead = False
                        , _feedItemIsPinned = False
                        }

-- | Simplistic unescape of XML entities. Unescaping the body is done
-- by tagsoup but often the title is also escaped and this we have to
-- handle ourselves.
unescapeXml :: Text -> Text
unescapeXml =
    T.replace "&amp;" "&"
    . T.replace "&gt;" ">"
    . T.replace "&lt;" "<"
    . T.replace "&mdash;" "—"
    . T.replace "&ndash;" "–"
    . T.replace "&quot;" "\""

getItemBody :: F.Item -> Maybe Text
getItemBody item = maybe tryHarder (Just . T.pack) (F.getItemSummary item)
  where
    tryHarder =
      case item of
        F.AtomItem i -> contentToStr <$> Atom.entryContent i
        _ -> Nothing

contentToStr :: Atom.EntryContent -> Text
contentToStr x =
  case x of
    Atom.TextContent  s -> T.replace "\n" "<br/>" . T.pack $ s
    Atom.HTMLContent  s -> T.pack s
    Atom.XHTMLContent s -> T.pack . strContent $ s
    s -> "<p>Could not interpret:</p><p>" <> tshow s <> "</p>"

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Simple transform from Maybe values to Either values.
m2e :: a -> Maybe b -> Either a b
m2e x = maybe (Left x) Right

fetchFeed :: MonadIO m => H.Manager -> Entity Feed -> m (Either Text (Entity Feed, F.Feed))
fetchFeed httpManager feedEntity@(Entity _ feed) = liftIO go
  where
    go = fetch `catch` (return . Left . (feedName <>) . T.pack . err)

    feedName =
      let t = feed ^. feedTitle
          u = tshow (feed ^. feedUrl)
      in  if t == "" then u <> ": " else t <> ": "

    err (H.InvalidUrlException s msg) = msg ++ s
    err (H.StatusCodeException (H.Status {..}) _ _) =
        "Got status " ++ show statusCode ++ " " ++ show statusMessage
    err H.ResponseTimeout = "Request timed out"
    err (H.FailedConnectionException host _) =
        "Could not connect to " ++ host
    err (H.FailedConnectionException2 host _ _ ex) =
        "Could not connect to " ++ host ++ " (" ++ show ex ++ ")"
    err (H.TooManyRedirects _) = "Too many redirects"
    err (H.UnparseableRedirect _) = "Could not parse redirect"
    err H.TooManyRetries = "Too many retries"
    err e = show e

    fetch =
      do
        request <- H.parseUrlThrow (show $ feed ^. feedUrl)
        response <- H.httpLbs request httpManager
        if H.responseStatus response == H.ok200
          then updateFeed' (H.responseBody response)
          else return $ Left "Invalid response"

    decodeBody = T.decodeUtf8With T.lenientDecode . BL.toStrict

    updateFeed' body =
      case F.parseFeedSource (decodeBody body) of
        Just rawFeed -> return $ Right (feedEntity, rawFeed)
        Nothing -> return $ Left "Could not parse feed"
