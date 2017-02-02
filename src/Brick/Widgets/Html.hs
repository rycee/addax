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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module implements a simple Brick widget for displaying HTML
-- documents.
module Brick.Widgets.Html
  ( HtmlView
  , htmlView
  , handleHtmlEvent
  , renderHtml
  , pvHtmlDoc
  , pvShowRaw
  ) where

import qualified Brick as B
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Char (isPunctuation, isSpace)
import           Data.Default (def)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import           Graphics.Vty.Picture
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree hiding (renderTree)
import           Text.PrettyPrint.Annotated.Leijen ((<+>), (</>))
import qualified Text.PrettyPrint.Annotated.Leijen as PP

data HtmlView n = HtmlView { _pvViewportName :: n
                           , _pvDoc :: [TagTree Text]
                           , _pvRawDoc :: Text
                           , _pvIdLinks :: Bool
                           , _pvShowRaw :: Bool
                           }

makeLenses ''HtmlView

instance Show (HtmlView n) where
  show _ = "HtmlView"

handleHtmlEvent :: Monad m => V.Event -> HtmlView n -> m (HtmlView n)
handleHtmlEvent ev html = return $
  case ev of
    V.EvKey (V.KChar 'g') [] -> over pvIdLinks not html
    V.EvKey (V.KChar 'r') [] -> over pvShowRaw not html
    _ -> html

htmlView :: n -> HtmlView n
htmlView name =
  HtmlView { _pvViewportName = name
           , _pvDoc = []
           , _pvRawDoc = ""
           , _pvIdLinks = False
           , _pvShowRaw = False
           }

pvHtmlDoc :: Lens' (HtmlView n) Text
pvHtmlDoc =
    lens _pvRawDoc (\ html txt -> html { _pvDoc = numberLinks . tagTree . parseTags $ txt
                                       , _pvRawDoc = txt
                                       })

data HtmlAnnotation = AnnotLink Text Int | AnnotAttrName B.AttrName
  deriving (Eq, Show)

fillSepWords :: Text -> PP.Doc a
fillSepWords = PP.fillSep . map (PP.text . T.unpack) . T.words

htmlEmphasis :: B.AttrName
htmlEmphasis = "html" <> "em"

htmlTh :: B.AttrName
htmlTh = "html" <> "th"

htmlTd :: B.AttrName
htmlTd = "html" <> "td"

htmlLink :: B.AttrName
htmlLink = "html" <> "link"

htmlPre :: B.AttrName
htmlPre = "html" <> "pre"

htmlCode :: B.AttrName
htmlCode = "html" <> "code"

htmlH1 :: B.AttrName
htmlH1 = "html" <> "h1"

htmlH2 :: B.AttrName
htmlH2 = "html" <> "h2"

htmlH3 :: B.AttrName
htmlH3 = "html" <> "h3"

htmlH4 :: B.AttrName
htmlH4 = "html" <> "h4"

htmlH5 :: B.AttrName
htmlH5 = "html" <> "h5"

htmlH6 :: B.AttrName
htmlH6 = "html" <> "h6"

htmlBlockquote :: B.AttrName
htmlBlockquote = "html" <> "blockquote"

htmlU :: B.AttrName
htmlU = "html" <> "u"

renderHtml :: HtmlView n -> B.Widget n
renderHtml pv =
    B.Widget B.Fixed B.Fixed $
      do
        let blocks =
              if (pv ^. pvShowRaw)
              then PP.string $ T.unpack (pv ^. pvRawDoc)
              else renderTrees (pv ^. pvDoc)
        img <- fmap (V.vertCat . map V.horizCat) (renderDoc blocks)
        return $ set B.imageL img def

renderDoc :: PP.Doc HtmlAnnotation -> B.RenderM n [[Image]]
renderDoc doc =
  do
    context <- B.getContext
    let lim = context ^. B.availWidthL
        simpleDoc = dropInitialWhiteSpace $ PP.renderPretty 1.0 lim doc
    go [] [def] simpleDoc [] []
  where
    go _ [] _ _ _ = error "impossible 1"
    go stk attrs@(attr:_) docElem row ls =
      case docElem of
        PP.SEmpty ->
            return . reverse $ reverse row : ls
        PP.SChar ch rest ->
            go stk attrs rest (V.char attr ch : row) ls
        PP.SText _ str rest ->
            go stk attrs rest (V.string attr str : row) ls
        PP.SLine ind rest ->
            go stk attrs rest [V.charFill attr ' ' ind 1] (reverse row : ls)
        PP.SAnnotStart a@(AnnotAttrName attrName) rest ->
          do
            attr' <- B.lookupAttrName attrName
            go (a:stk) (attr <> attr' : attrs) rest row ls
        PP.SAnnotStart a rest -> go (a:stk) attrs rest row ls
        PP.SAnnotStop rest ->
          case stk of
            (AnnotAttrName _ : xs) -> go xs (tail attrs) rest row ls
            (_ : xs) -> go xs attrs rest row ls
            _ -> error "impossible 2"

-- | Trim initial white space.
dropInitialWhiteSpace :: PP.SimpleDoc t -> PP.SimpleDoc t
dropInitialWhiteSpace (PP.SLine _ rest) = dropInitialWhiteSpace rest
dropInitialWhiteSpace (PP.SText _ t rest) =
  let t' = dropWhile isSpace t
  in  if t' == ""
      then dropInitialWhiteSpace rest
      else PP.SText (length t') t rest
dropInitialWhiteSpace docElem = docElem

-- | Adds an "addax-id" attribute to all "a" tags. Its value indicates
-- the unique index of the link.
numberLinks :: [TagTree Text] -> [TagTree Text]
numberLinks = flip evalState (0 :: Int) . mapM go
  where
    go (TagBranch tag as ts)
      | tag == "a" || tag == "img" =
        do
          n <- get
          modify' (+ 1)
          return $ TagBranch tag (("addax-id", T.pack (show n)) : as) ts
      | otherwise =
        do
          ts' <- mapM go ts
          return $ TagBranch tag as ts'
    go leaf@(TagLeaf _) = return leaf

renderTreesWith :: (TagTree Text -> PP.Doc a) -> [TagTree Text] -> PP.Doc a
renderTreesWith rndr = go
  where
    go [] = PP.empty
    go [x] = rndr x
    go (x:y:rest) =
      case (x, y) of
        (_, TagLeaf (TagText t))
          | not (T.null t) && isPunctuation (T.head t) -> rx PP.<> rrest
        (TagLeaf (TagText t), _)
          | not (T.null t) && isPunctuation (T.last t) -> rx PP.<> rrest
        _ -> rx </> rrest
      where
        rx = rndr x
        rrest = go (y:rest)

-- | Transforms a given tag tree to have only lower case tag names.
lowerCaseTags :: TagTree Text -> TagTree Text
lowerCaseTags (TagBranch tag as ts) =
    TagBranch (T.toLower tag) as ts
--    TagBranch (T.toLower tag) as (map lowerCaseTags ts)
lowerCaseTags (TagLeaf (TagOpen tag as)) =
    TagLeaf (TagOpen (T.toLower tag) as)
lowerCaseTags t = t

renderTrees :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderTrees = renderTreesWith (renderTree . cleanupTree . lowerCaseTags)

-- Special cases to handle:
--
--  - <li><p>…</p></li> → <li>…</li>
--
--  - <li>foo<li>bar</li></li> → <li>foo</li><li>bar</li>
cleanupTree :: TagTree Text -> TagTree Text
cleanupTree (TagBranch "li" as [TagBranch "p" _ ts]) = TagBranch "li" as ts
cleanupTree (TagBranch tag as ts) = TagBranch tag as (map cleanupTree ts)
cleanupTree t = t

renderTree :: TagTree Text -> PP.Doc HtmlAnnotation
renderTree (TagBranch "a" as ts) = renderLink (const $ renderTrees ts) "href" as
renderTree (TagBranch "b" _ ts) = renderEm ts
renderTree (TagBranch "blockquote" _ ts) = breakAround (renderBlockquote ts)
renderTree (TagBranch "br" _ ts) = PP.linebreak PP.<> renderTrees ts
renderTree (TagBranch "cite" _ ts) = renderEm ts
renderTree (TagBranch "code" _ ts) = renderTagCode ts
renderTree (TagBranch "dd" _ ts) = breakAround (renderLi '-' ts)
renderTree (TagBranch "del" _ ts) = renderStrike ts
renderTree (TagBranch "div" _ ts) = renderTrees ts
renderTree (TagBranch "dl" _ ts) = breakAround (renderTrees ts)
renderTree (TagBranch "dt" _ ts) = breakAround (renderEm ts)
renderTree (TagBranch "em" _ ts) = renderEm ts
renderTree (TagBranch "h1" _ ts) = renderHeader "==" htmlH1 ts
renderTree (TagBranch "h2" _ ts) = renderHeader "--" htmlH2 ts
renderTree (TagBranch "h3" _ ts) = renderHeader "-" htmlH3 ts
renderTree (TagBranch "h4" _ ts) = renderHeader "" htmlH4 ts
renderTree (TagBranch "h5" _ ts) = renderHeader "" htmlH5 ts
renderTree (TagBranch "h6" _ ts) = renderHeader "" htmlH6 ts
renderTree (TagBranch "i" _ ts) = renderEm ts
renderTree (TagBranch "img" as _) = renderImgLink as
renderTree (TagBranch "li" _ ts) = breakAround (renderLi '•' ts)
renderTree (TagBranch "object" _ ts) = renderTrees ts
renderTree (TagBranch "ol" _ ts) = breakAround (renderOl ts)
renderTree (TagBranch "p" _ ts) = breakAround (renderTrees ts)
renderTree (TagBranch "param" _ ts) = renderTrees ts
renderTree (TagBranch "pre" _ ts) = renderPre ts
renderTree (TagBranch "q" _ ts) = PP.enclose (PP.char '"') (PP.char '"') $ renderTrees ts
renderTree (TagBranch "s" _ ts) = renderStrike ts
renderTree (TagBranch "small" _ ts) = renderTrees ts
renderTree (TagBranch "span" _ ts) = renderTrees ts
renderTree (TagBranch "strike" _ ts) = renderStrike ts
renderTree (TagBranch "strong" _ ts) = renderEm ts
renderTree (TagBranch "sup" _ ts) = renderTrees ts
renderTree (TagBranch "table" _ ts) = breakAround (renderTrees ts)
renderTree (TagBranch "tbody" _ ts) = renderTrees ts
renderTree (TagBranch "td" _ ts) = renderTableCell htmlTd ts
renderTree (TagBranch "th" _ ts) = renderTableCell htmlTh ts
renderTree (TagBranch "tr" _ ts) = renderTableRow ts
renderTree (TagBranch "tt" _ ts) = renderTagCode ts
renderTree (TagBranch "u" _ ts) = renderU ts
renderTree (TagBranch "ul" _ ts) = breakAround (renderUl ts)
renderTree (TagBranch tag _ ts) = PP.angles (PP.text $ T.unpack tag) </> renderTrees ts
renderTree (TagLeaf leaf) = renderLeaf leaf

breakAround :: PP.Doc a -> PP.Doc a
breakAround doc = PP.linebreak PP.<> doc PP.<> PP.linebreak

renderStrike :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderStrike ts = strike <+> renderTrees ts <+> strike
  where
    strike = PP.text "-"

renderTableCell :: B.AttrName -> [TagTree Text] -> PP.Doc HtmlAnnotation
renderTableCell attrName ts =
    PP.annotate (AnnotAttrName attrName)
    $ PP.enclose (PP.char '|') (PP.char '|')
    $ renderTrees ts

renderTableRow :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderTableRow ts = renderTrees ts PP.<> PP.linebreak

renderHeader :: String -> B.AttrName -> [TagTree Text] -> PP.Doc HtmlAnnotation
renderHeader mark attrName ts =
    PP.annotate (AnnotAttrName attrName)
    $ breakAround
    $ mark' <+> renderTrees ts <+> mark'
  where
    mark' = PP.text mark

renderEm :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderEm = PP.annotate (AnnotAttrName htmlEmphasis) . renderTrees

renderU :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderU = PP.annotate (AnnotAttrName htmlU) . renderTrees

renderPre :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderPre =
    breakAround
    . PP.annotate (AnnotAttrName htmlPre)
    . PP.indent 4
    . renderPreTrees
  where
    renderPreTrees = PP.hcat . map renderPreTree

renderPreTree :: TagTree Text -> PP.Doc HtmlAnnotation
renderPreTree (TagLeaf leaf) =
  case leaf of
    TagText str -> PP.string (T.unpack str)
    _ -> PP.empty
renderPreTree (TagBranch _ _ ts) = PP.hcat . map renderPreTree $ ts

renderTagCode :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderTagCode = PP.annotate (AnnotAttrName htmlCode) . renderTrees

renderBlockquote :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderBlockquote =
    PP.annotate (AnnotAttrName htmlBlockquote) . PP.indent 4 . renderTrees

renderUl :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderUl = PP.indent 2 . renderUlTrees
  where
    renderUlTrees = renderTreesWith renderUlTree
    renderUlTree (TagBranch "li" _ ts) = breakAround (renderLi '•' ts)
    renderUlTree n = renderTree n

renderOl :: [TagTree Text] -> PP.Doc HtmlAnnotation
renderOl = PP.indent 2 . renderOlTrees
  where
    renderOlTrees = renderTreesWith renderOlTree
    renderOlTree (TagBranch "li" _ ts) = breakAround (renderLi '#' ts)
    renderOlTree n = renderTree n

renderLi :: Char -> [TagTree Text] -> PP.Doc HtmlAnnotation
renderLi ch = (PP.char ch <+>) . PP.hang 0 . renderTrees

renderImgLink :: [(Text, Text)] -> PP.Doc HtmlAnnotation
renderImgLink as = renderLink (maybe PP.empty title) "src" as
  where
    title (url, _) = PP.angles (fillSepWords imgTitle)
      where
        imgTitle = maybe url id $ lookup "title" as

renderLink :: (Maybe (Text, Int) -> PP.Doc HtmlAnnotation)
           -> Text
           -> [(Text, Text)]
           -> PP.Doc HtmlAnnotation
renderLink titleRender urlAttrName as = annotateLink (titleRender linkPair)
 where
    annotateAttr = PP.annotate (AnnotAttrName htmlLink)
    linkAnnotation = AnnotLink <$> lookup urlAttrName as <*> lookupInt "addax-id" as
    linkPair = (,) <$> lookup urlAttrName as <*> lookupInt "addax-id" as
    annotateLink = maybe id (\a -> annotateAttr . PP.annotate a) linkAnnotation
    lookupInt k = fmap (read . T.unpack) . lookup k

renderLeaf :: Tag Text -> PP.Doc HtmlAnnotation
renderLeaf (TagClose _) = PP.empty
renderLeaf (TagComment _) = PP.empty
renderLeaf (TagOpen "!doctype" _) = PP.empty
renderLeaf (TagOpen "blockquote" _) = PP.empty
renderLeaf (TagOpen "br" _) = PP.linebreak
renderLeaf (TagOpen "hr" _) = breakAround (PP.text "-----------")
renderLeaf (TagOpen "img" as) = renderImgLink as
renderLeaf (TagText txt) = fillSepWords txt
renderLeaf l = PP.brackets . PP.text . show $ l
