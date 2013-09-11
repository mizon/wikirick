module Wikirick.AppHandler
  ( handleArticle
  , handleEdit
  ) where

import Control.Monad.CatchIO
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import Snap
import Snap.Snaplet.Heist
import Text.XmlHtml hiding (render)

import Wikirick.Application
import Wikirick.Repository
import Wikirick.Import
import Wikirick.URLMapper
import Wikirick.Util
import qualified Wikirick.View as V

handleFrontPage :: AppHandler ()
handleFrontPage = doHandleArticle "FrontPage"

handleArticle :: AppHandler ()
handleArticle = do
  title <- textParam "title"
  doHandleArticle title

doHandleArticle :: T.Text -> AppHandler ()
doHandleArticle title = do
  article <- try $ with repo $ fetchArticle title
  either newArticle openArticle article
  where
    newArticle ArticleNotFound = redirectTo $ EditPath $ def & articleTitle .~ title

    openArticle article
        = isXHR (renderArticle $ articleSplices article)
      <|> renderFull (articleSplices article)

    renderArticle = renderSplices "article"
    renderFull = renderSplices "base"

articleSplices :: Article -> [(T.Text, I.Splice AppHandler)]
articleSplices a =
  [ ("wiki:title", I.textSplice $ a ^. articleTitle)
  , ("wiki:content", I.callTemplate "article" [])
  , ("wiki:sections", sections)
  , ("wiki:navigation", V.navigation $ a ^. articleTitle)
  ] where
    sections = pure
      [ Element "section" []
        [ Element "h2" [] [TextNode "What is this?"]
        , Element "p" [] [TextNode "Haskell"]
        ]
      ]

handleEdit :: AppHandler ()
handleEdit = do
  title <- textParam "title"
  article <- try $ with repo $ fetchArticle title
  let article' = either (newArticle title) id article
      splices = editorSplices $ article' ^. articleTitle
  isXHR (renderEditor splices) <|> renderFull splices
  where
    newArticle title ArticleNotFound = def & articleTitle .~ title

    renderEditor = renderSplices "editor"
    renderFull = renderSplices "base"

handleSource :: AppHandler ()
handleSource = isXHR renderSource <|> renderFull where
  renderSource = undefined
  renderFull = undefined

editorSplices :: T.Text -> [(T.Text, I.Splice AppHandler)]
editorSplices title =
  [ ("wiki:title", I.textSplice title)
  , ("wiki:content", I.callTemplate "editor" [])
  , ("wiki:source", I.textSplice "foo")
  , ("wiki:navigation", V.navigation title)
  ]

isXHR :: AppHandler () -> AppHandler ()
isXHR h = getsRequest (getHeader "X-Requested-With") >>= \case
  Just "XMLHttpRequest" -> h
  _ -> pass

renderSplices :: BS.ByteString -> [(T.Text, I.Splice AppHandler)] -> AppHandler ()
renderSplices name splices = heistLocal (I.bindSplices splices) $ render name
