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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Addax.Ui (runAddaxUi) where

-- import           Database.Persist (HasPersistBackend(persistBackend))

import           Addax.About (aboutText)
import           Addax.Config
import           Addax.Interval (readInterval)
import           Addax.Types
import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Dialog as B
import qualified Brick.Widgets.Edit as B
import           Brick.Widgets.Html (HtmlView, htmlView, renderHtml, pvHtmlDoc, pvShowRaw)
import qualified Brick.Widgets.List as B
import           Control.Lens
import           Control.Monad (forM)
import           Control.Monad.Logger (MonadLoggerIO, runNoLoggingT)
import           Control.Monad.Trans (liftIO)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Database.Persist ((==.))
import qualified Database.Persist as P
import           Database.Persist.Sql (SqlBackend)
import qualified Database.Persist.Sql as P
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input
import           System.Process (readProcessWithExitCode)
import           Text.URI (URI, parseURI)

data AddaxNames = IndexItemsName
                | HtmlViewName
                | ItemBodyName
                | AboutPaneName

                | UrlEditorName
                | TitleEditorName
                | IntervalEditorName
                deriving (Eq, Ord, Show)

data EditFeedState =
  EditFeedState { _editFeedFeedId :: Maybe FeedId
                , _editFeedDialog :: B.Dialog Bool
                , _editFeedUrlEditor :: B.Editor Text AddaxNames
                , _editFeedTitleEditor :: B.Editor Text AddaxNames
                , _editFeedIntervalEditor :: B.Editor Text AddaxNames
                , _editFeedFocusRing :: B.FocusRing AddaxNames
                }

makeLenses ''EditFeedState

data View =
    ViewIndex
  | ViewAbout
  | ViewHelp
  | ViewEditFeed EditFeedState

-- | An item that may appear in the index list.
data Item = ListHead FeedId Feed Int | ListChild Feed FeedItem

data UiState =
  UiState { _sqlBackend :: SqlBackend
          , _config :: AddaxConfig
          , _curView :: View

            -- | Total number of unread items.
          , _totalNumUnread :: Int

            -- | Number of feeds due for update.
          , _numUpdateDue :: Int

            -- | Currently selected feed.
          , _indexFeedId :: Maybe FeedId

            -- | The index items.
          , _indexItems :: B.List AddaxNames Item

            -- | Body currently being displayed in the index.
          , _indexBody :: HtmlView AddaxNames
          }

makeLenses ''UiState

-- instance HasPersistBackend UiState SqlBackend where
--   persistBackend = dbBackend

helpText :: Text
helpText =
    T.unlines [ ""
              , "  Up        - Go to previous item in index"
              , "  Down      - Go to next item in index"
              , "  PgUp      - Go one page up in index"
              , "  PgDown    - Go one page down in index"
              , "  Right     - Expand current folder"
              , "  Left      - Collapse current folder"
              , "  n         - Go to next unread item"
              , "  o         - Open current item in browser"
              , ""
              , "  Space     - Scroll forwards in body"
              , "  Backspace - Scroll backwards in body"
              ]

topBar :: UiState -> B.Widget n
topBar st =
    B.hBox [ B.txt "q:Quit  i:Index  ?:Help  @:About"
           , B.vLimit 1 (B.fill ' ')
           , B.str (show $ st ^. totalNumUnread) <+> B.txt " unread " <+> B.txt "/"
             <+> B.str (show $ st ^. numUpdateDue) <+> B.txt " to update"
           ]

tshow :: Show a => a -> Text
tshow = T.pack . show

itemRead :: B.AttrName
itemRead = "item" <> "read"

itemUnread :: B.AttrName
itemUnread = "item" <> "unread"

indexWidget :: UiState -> B.Widget AddaxNames
indexWidget st = indexList <=> body <=> indexBar
  where
    indexBar = B.txt "n:Next unread  /:Search  o:Open  u:Update"

    curFeedTitle =
      case B.listSelectedElement (st ^. indexItems) of
        Nothing -> ""
        Just (_, ListHead _ feed _) -> feed ^. feedTitle
        Just (_, ListChild feed _) -> feed ^. feedTitle

    indexListLabel
      | T.null curFeedTitle = B.txt "Feeds"
      | otherwise = B.txt "Feed – " <+> B.txt curFeedTitle

    indexList =
        B.borderWithLabel indexListLabel
        $ B.vLimit 10
        $ B.renderList renderElement True (st ^. indexItems)

    renderElement _ (ListHead _ feed numUnread) =
      let
        title = feed ^. feedTitle
        neverUpdatedTitle = tshow $ feed ^. feedUrl
        shownTitle = if title == "" then neverUpdatedTitle else title
        widget = B.txt shownTitle <+> B.vLimit 1 (B.fill ' ') <+> B.txt (tshow numUnread <> " unread")
      in
        widget
    renderElement _ (ListChild _ feedItem) =
      let
        title = feedItem ^. feedItemTitle
        widget = B.txt "  " <+> B.txt (if title == "" then "Untitled" else title)
        isRead = feedItem ^. feedItemIsRead
        attr = if isRead then itemRead else itemUnread
      in
        B.withAttr attr widget

    body =
        B.border
        . B.viewport ItemBodyName B.Vertical
        $ bodyWidget st (B.listSelectedElement (st ^. indexItems))

listSelectedElementL :: Getter (B.List n e) (Maybe e)
listSelectedElementL = to (\ lst -> snd <$> B.listSelectedElement lst)

vpItemBodyScroll :: B.ViewportScroll AddaxNames
vpItemBodyScroll = B.viewportScroll ItemBodyName

vpAboutScroll :: B.ViewportScroll AddaxNames
vpAboutScroll = B.viewportScroll AboutPaneName

bodyWidget :: UiState -> Maybe (Int, Item) -> B.Widget AddaxNames
bodyWidget _ Nothing = B.txt "Press 'a' to add a feed…"
bodyWidget st (Just (_, ListHead _ feed numUnread)) =
    headerFmt "Feed title" (empty "Unknown" $ feed ^. feedTitle)
    <=>
    headerFmt "Feed URL" (tshow $ feed ^. feedUrl)
    <=>
    headerFmt "Last updated" (maybe "Never" tshow $ feed ^. feedUpdatedAt)
    <=>
    headerFmt "Update interval" (tshow $ feedIntervalWithDefault defInterval feed)
    <=>
    headerFmt "Next update" (tshow $ feedNextUpdateAt defInterval feed)
    <=>
    headerFmt "Number of unread items" (tshow numUnread)
  where
    defInterval = st ^. (config . confDefaultInterval)
    empty d x = if x == "" then d else x
bodyWidget st (Just (_, ListChild _ feedItem)) =
    headerFmt "Title" (feedItem ^. feedItemTitle)
    <=?>
    headerFmtM "Author" (feedItem ^. feedItemAuthor)
    <=?>
    headerFmtM "Published" (tshow <$> feedItem ^. feedItemPublishedAt)
    <=>
    headerFmt "Downloaded" (tshow $ feedItem ^. feedItemDownloadedAt)
    <=>
    B.hBorder
    <=>
    renderHtml (st ^. indexBody)

(<=?>) :: B.Widget n -> Maybe (B.Widget n) -> B.Widget n
a <=?> Just b = a <=> b
a <=?> Nothing = a

headerFmt :: Text -> Text -> B.Widget n
headerFmt key val = B.txt key <+> B.txt ": " <+> B.txt val

headerFmtM :: Monad m => Text -> m Text -> m (B.Widget n)
headerFmtM key val = headerFmt key <$> val

drawUi :: UiState -> [B.Widget AddaxNames]
drawUi st = [ topBar st <=> widget (st ^. curView) ]
  where
    widget ViewIndex = indexWidget st
    widget ViewHelp = B.txt helpText
    widget ViewAbout = B.viewport AboutPaneName B.Both (B.txt aboutText)
    widget (ViewEditFeed eSt) = editFeedWidget eSt

editFeedView :: Maybe FeedId -> Feed -> View
editFeedView feedId feed =
    ViewEditFeed $
        EditFeedState { _editFeedFeedId = feedId
                      , _editFeedDialog = dlg
                      , _editFeedUrlEditor = urlEditor
                      , _editFeedTitleEditor = titleEditor
                      , _editFeedIntervalEditor = intervalEditor
                      , _editFeedFocusRing = focusRing
                      }
  where
    -- (feedId, feed) = maybe (Nothing, emptyFeed) id $ mfeed
    dlg = B.dialog (Just "Edit feed") dlgBtns 50
    dlgBtns = Nothing -- Just (0, [("Add", True), ("Cancel", False)])
    lnrndr = B.txt . T.concat
    rndrDef = B.txt "[Default]"
    urlEditor =
        B.editor UrlEditorName
                 lnrndr
                 (Just 1)
                 (tshow $ feed ^. feedUrl)
    titleEditor =
        B.editor TitleEditorName
                 lnrndr
                 (Just 1)
                 (feed ^. feedTitle)
    intervalEditor =
        B.editor IntervalEditorName
                 (\ ls -> if null ls then rndrDef else lnrndr ls)
                 (Just 1)
                 (maybe "" tshow $ feed ^. feedUpdateInterval)
    focusRing = B.focusRing [ UrlEditorName
                            , TitleEditorName
                            , IntervalEditorName
                            ]

editFeedWidget :: EditFeedState -> B.Widget AddaxNames
editFeedWidget (EditFeedState {..}) = B.renderDialog _editFeedDialog wdgt
  where
    form editor check =
        (B.withFocusRing _editFeedFocusRing B.renderEditor editor)
        <+>
        (B.txt " ")
        <+>
        (B.txt $ if check (T.unpack . mconcat . B.getEditContents $ editor) then "OK" else "BAD")
    alignedTxt t = B.hLimit 16 (B.txt t <+> B.vLimit 1 (B.fill ' '))
    urlCheck = maybe False (const True) . parseURI
    titleCheck = const True
    intervalCheck = either (const False) (const True) . readInterval
    wdgt =
        (alignedTxt "Feed URL" <+> form _editFeedUrlEditor urlCheck)
        <=>
        (alignedTxt "Feed title" <+> form _editFeedTitleEditor titleCheck)
        <=>
        (alignedTxt "Update interval" <+> form _editFeedIntervalEditor intervalCheck)
        <=>
        (B.vLimit 1 (B.fill ' ') <+> B.txt "Cancel (ESC) | Confirm (Enter)")

appEvent :: UiState
         -> B.BrickEvent AddaxNames ()
         -> B.EventM AddaxNames (B.Next UiState)
appEvent st ev = viewEvent (st ^. curView)
  where
    switchView v = B.continue $ set curView v st

    handleStandardEvents =
      case ev of
        B.VtyEvent (EvKey (KChar '?') []) -> switchView ViewHelp
        B.VtyEvent (EvKey (KChar '@') []) -> switchView ViewAbout
        -- B.VtyEvent (EvKey (KChar 'a') []) -> switchView (ViewEditFeed emptyFeed manageFeedDialog)
        B.VtyEvent (EvKey (KChar 'i') []) -> switchView ViewIndex
        B.VtyEvent (EvKey (KChar 'q') []) -> B.halt st
        _ -> B.continue st

    isStandardEvent =
        case ev of
          B.VtyEvent (EvKey (KChar ch) []) -> ch `elem` ['?', '@', 'i', 'q']
          _ -> False

    selectedFeed lst =
      do
        curIdx <- lst ^. B.listSelectedL
        ListHead feedKey _ _ <- V.find isListHead . V.reverse . V.take (curIdx + 1) $ lst ^. B.listElementsL
        return feedKey
      where
        isListHead (ListHead _ _ _) = True
        isListHead (ListChild _ _) = False

    viewEvent ViewAbout =
      case ev of
        B.VtyEvent (EvKey (KChar ' ') []) ->
            B.vScrollPage vpAboutScroll B.Down >> B.continue st
        B.VtyEvent (EvKey KBS []) ->
            B.vScrollPage vpAboutScroll B.Up >> B.continue st
        _ -> handleStandardEvents
    viewEvent ViewIndex =
      case ev of
        B.VtyEvent (EvKey KRight []) ->
          case st ^. (indexItems . listSelectedElementL) of
            Just (ListHead feedKey _ _) ->
                B.continue
                =<< liftIO . populateList
                =<< return (set indexFeedId (Just feedKey) st)
            _ -> B.continue st
        B.VtyEvent (EvKey KLeft []) ->
            B.continue
            =<< liftIO . populateList
            =<< return (set indexFeedId (selectedFeed (st ^. indexItems)) st)
        B.VtyEvent (EvKey (KChar 'e') []) ->
          case st ^. (indexItems . listSelectedElementL) of
            Just (ListHead feedId feed _) ->
                switchView (editFeedView (Just feedId) feed)
            Just (ListChild feed _) ->
                switchView (editFeedView Nothing feed)
            Nothing -> B.continue st
        B.VtyEvent (EvKey (KChar 'n') []) -> B.continue =<< gotoNextUnread st
        B.VtyEvent (EvKey (KChar 'o') []) ->
          do
            liftIO $ openSelectedItem (st ^. indexItems)
            B.continue st
        B.VtyEvent (EvKey (KChar 'r') []) ->
            B.continue $ over (indexBody . pvShowRaw) not st
        B.VtyEvent (EvKey KBS []) ->
            B.vScrollPage vpItemBodyScroll B.Up >> B.continue st
        B.VtyEvent (EvKey (KChar ' ') []) ->
            B.vScrollPage vpItemBodyScroll B.Down >> B.continue st
        _ | isStandardEvent -> handleStandardEvents
        B.VtyEvent vtyEv ->
            B.continue
            =<< updateIndex st (\st' -> B.handleEventLensed st' indexItems B.handleListEvent vtyEv)
        _ -> B.continue st
    viewEvent (ViewEditFeed eSt) =
      case ev of
        B.VtyEvent (EvKey KEsc []) -> switchView ViewIndex
        B.VtyEvent (EvKey KEnter []) ->
            B.continue =<< liftIO (editFeedUpdateFeed eSt st)
        B.VtyEvent (EvKey KDown []) ->
          let eSt' = over editFeedFocusRing B.focusNext eSt
          in  B.continue $ set curView (ViewEditFeed eSt') st
        B.VtyEvent vtyEv ->
            editFeedEvent vtyEv (B.focusGetCurrent $ eSt ^. editFeedFocusRing) eSt
        _ -> B.continue st
    viewEvent _ = handleStandardEvents

    editFeedEvent _ Nothing _ = B.continue st
    editFeedEvent vtyEv (Just name) eSt
      | name == UrlEditorName =
          do
            eSt' <- B.handleEventLensed eSt editFeedUrlEditor B.handleEditorEvent vtyEv
            B.continue $ set curView (ViewEditFeed eSt') st
      | name == TitleEditorName =
          do
            eSt' <- B.handleEventLensed eSt editFeedTitleEditor B.handleEditorEvent vtyEv
            B.continue $ set curView (ViewEditFeed eSt') st
      -- | name == EditFeedDialogName =
      --     do
      --       eSt' <- B.handleEventLensed eSt editFeedDialog B.handleEditorEvent ev
      --       B.continue $ set curView (ViewEditFeed eSt') st
      | otherwise = B.continue st
          -- do
          --   eSt' <- B.handleEventLensed eSt editFeedDialog B.handleEditorEvent ev
          --   B.continue $ set curView (ViewEditFeed eSt') st

editFeedUpdateFeed :: EditFeedState -> UiState -> IO UiState
editFeedUpdateFeed eSt st =
  case validated of
    Left msg -> putStrLn msg >> return st
    Right work -> work >> return (set curView ViewIndex st)
  where
    validated =
      do
        url <- maybe (Left "Invalid URL") Right . parseURI $ urlText
        return $ worker url

    urlText = T.unpack $ mconcat $ B.getEditContents (eSt ^. editFeedUrlEditor)

    worker :: URI -> IO ()
    worker url = runNoLoggingT $ P.runSqlConn (sql url) (st ^. sqlBackend)

    sql :: MonadLoggerIO m => URI -> P.SqlPersistT m ()
    sql url =
      case (eSt ^. editFeedFeedId) of
        Nothing -> addFeed Nothing Nothing url
        Just feedId -> P.update feedId [ FeedUrl P.=. url ]

-- handleIndexItemsEvent :: Event -> UiState -> EventM n UiState
-- handleIndexItemsEvent 

updateIndex :: UiState -> (UiState -> B.EventM AddaxNames UiState) -> B.EventM AddaxNames UiState
updateIndex st up =
  do
    let before = st ^. (indexItems . B.listSelectedL)
    st' <-  up st
    let after = st' ^. (indexItems . B.listSelectedL)
        after' = B.listSelectedElement (st' ^. indexItems)
    case (before == after, after') of
      (False, Just (idx, ListChild feed feedItem)) ->
        do
          B.vScrollToBeginning vpItemBodyScroll
          liftIO $ P.runSqlConn (updateItemRead True feedItem) (st' ^. sqlBackend)
          return $ updateHtmlView $
              set (indexItems . B.listElementsL . ix idx)
                  (ListChild feed $ set feedItemIsRead True feedItem)
                  st'
      (False, Just (_, ListHead _ _ _)) ->
        do
          B.vScrollToBeginning vpItemBodyScroll
          return $ updateHtmlView st'
      _ -> return st' -- $ updateHtmlView st'

-- moveIndex :: UiState -> Event -> B.EventM UiState
-- moveIndex st ev =
--   do
--     let before = st ^. (items . B.listSelectedL)
--     st' <- B.handleEventLensed st items ev
--     let after = st' ^. (items . B.listSelectedL)
--         after' = B.listSelectedElement (st' ^. items)
--     case (before == after, after') of
--       (False, Just (idx, ListChild feedItem)) ->
--         do
--           liftIO $ P.runSqlConn (updateItemRead True feedItem) (st' ^. sqlBackend)
--           return $ updateHtmlView $
--               set (items . B.listElementsL . ix idx)
--                   (ListChild $ set feedItemIsRead True feedItem)
--                   st'
--       _ -> return $ updateHtmlView st'


updateHtmlView :: UiState -> UiState
updateHtmlView st =
  case B.listSelectedElement (st ^. indexItems) of
    Nothing -> st
    Just (_, ListHead _ _ _) -> set (indexBody . pvHtmlDoc) "" st
    Just (_, ListChild _ feedItem) -> set (indexBody . pvHtmlDoc) (feedItem ^. feedItemBody) st

-- | Opens the selected item. If it is a feed then open the feed web
-- URL and if it is a feed item, open the item URL.
openSelectedItem :: B.List n Item -> IO ()
openSelectedItem lst = maybe (return ()) (spawn . show) murl
  where
    murl =
      case B.listSelectedElement lst of
        Just (_, ListHead _ feed _) -> Just (feed ^. feedUrl)
        Just (_, ListChild _ feedItem) -> Just (feedItem ^. feedItemUrl)
        Nothing -> Nothing
    spawn url =
      do
        (_, _, _) <- readProcessWithExitCode "xdg-open" [url] ""
        return ()

gotoNextUnread :: UiState -> B.EventM AddaxNames UiState
gotoNextUnread st =
  case nextUnread (st ^. indexItems) of
    Nothing ->
      case nextUnreadFeed (st ^. indexItems) of
        Nothing -> return st
        Just feedKey ->
          do
            st' <- liftIO $ populateList (set indexFeedId (Just feedKey) st)
            gotoNextUnread st'
    Just lst -> updateIndex st (return . set indexItems lst)

-- | Moves selection to the next unread item.
nextUnread :: B.List n Item -> Maybe (B.List n Item)
nextUnread lst = fmap (\ i -> set B.listSelectedL (Just i) lst) idx
  where
    idx = V.findIndex isUnread (lst ^. B.listElementsL)
    isUnread (ListHead _ _ _) = False
    isUnread (ListChild _ item) = not (item ^. feedItemIsRead)

-- | Finds feed key of next unread feed.
nextUnreadFeed :: B.List n Item -> Maybe FeedId
nextUnreadFeed lst = bleh listHead
  where
    listHead = V.find isUnread (lst ^. B.listElementsL)
    bleh (Just (ListHead feedKey _ _)) = Just feedKey
    bleh _ = Nothing
    isUnread (ListHead _ _ numUnread) = numUnread > 0
    isUnread (ListChild _ _) = False

attrMap :: UiState -> B.AttrMap
attrMap _ = B.attrMap defAttr
    [ (B.listSelectedAttr,    black `B.on` white)
    , (B.buttonAttr,          white `B.on` blue)
    , (B.buttonSelectedAttr,  blue `B.on` white)
    , (itemUnread,            B.fg brightWhite)
    , ("html" <> "link",      B.fg blue)
    , ("html" <> "em",        B.fg brightWhite)
    , ("html" <> "pre",       B.fg green)
    , ("html" <> "code",      B.fg green)
    , ("html" <> "h1",        B.fg brightWhite)
    , ("html" <> "h2",        B.fg brightWhite)
    , ("html" <> "h3",        B.fg brightWhite)
    ]

appCursor :: UiState -> [B.CursorLocation AddaxNames] -> Maybe (B.CursorLocation AddaxNames)
appCursor st = go (st ^. curView)
  where
    go ViewIndex = B.showCursorNamed IndexItemsName
    go (ViewEditFeed eSt) =
        B.focusRingCursor (const $ eSt ^. editFeedFocusRing) st
    go _ = B.neverShowCursor st

addaxApp :: B.App UiState () AddaxNames
addaxApp =
    B.App { B.appDraw = drawUi
          , B.appChooseCursor = appCursor
          , B.appHandleEvent = appEvent
          , B.appStartEvent = liftIO . populateList
          , B.appAttrMap = attrMap
          -- , B.appLiftVtyEvent = id
          }

initialState :: AddaxConfig -> SqlBackend -> UiState
initialState cfg backend =
    UiState { _sqlBackend = backend
            , _config = cfg
            , _curView = ViewIndex
            , _totalNumUnread = 0
            , _numUpdateDue = 0
            , _indexFeedId = Nothing
            , _indexItems = B.list IndexItemsName mempty 1
            , _indexBody = htmlView HtmlViewName
            }

selectItem :: FeedItem -> UiState -> UiState
selectItem feedItem st =
    set (indexItems . B.listSelectedL) idx st
  where
    idx = V.findIndex p (st ^. (indexItems . B.listElementsL))
    p (ListChild _ feedItem') =
        (feedItem ^. feedItemUrl) == (feedItem' ^. feedItemUrl)
    p _ = False

populateList :: UiState -> IO UiState
populateList st =
  do
    listItems <- P.runSqlConn fetchItems backend
    toUpdate <- P.runSqlConn (feedsToUpdate defInterval True) backend
    let listItems' = V.fromList (concat listItems)
        totNumUnread = V.filter itemIsUnread listItems'
        idx = if V.null listItems'
              then Nothing
              else maybe (Just 0) Just (V.findIndex isCurFeedEntry listItems')
--        idx = if V.null listItems' then Nothing else Just 0
    return
        . set totalNumUnread (V.length totNumUnread)
        . set numUpdateDue (length toUpdate)
        . set (indexItems . B.listSelectedL) idx
        . set (indexItems . B.listElementsL) listItems'
        $ st
  where
    defInterval = st ^. (config . confDefaultInterval)
    backend = st ^. sqlBackend
    curFeedKey = st ^. indexFeedId
    isCurFeedEntry (ListHead feedKey _ _) = Just feedKey == curFeedKey
    isCurFeedEntry _ = False
    itemIsUnread (ListHead _ _ numUnread) = numUnread > 0
    itemIsUnread (ListChild _ feedItem) = not (feedItem ^. feedItemIsRead)
    fetchItems =
      do
        feeds <- getFeedWithUnreadCount -- P.selectList [] [ P.Asc FeedTitle ]
        forM feeds $ \ (P.Entity feedKey feed, numUnread) ->
          do
--            unreadFeedItems <- P.selectList [ FeedItemFeed ==. feedKey, FeedItemIsRead ==. False ] []
            feedItems <-
              if Just feedKey == st ^. indexFeedId
              then P.selectList [ FeedItemFeed ==. feedKey, FeedItemIsRead ==. False ] []
              else return []
            return
                . (ListHead feedKey feed numUnread :)
                . map (ListChild feed . P.entityVal)
                $ feedItems

runAddaxUi :: MonadLoggerIO m => AddaxConfig -> SqlBackend -> m ()
runAddaxUi cfg db =
    liftIO $ B.defaultMain addaxApp (initialState cfg db) >> return ()
