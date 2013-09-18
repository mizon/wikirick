module Wikirick.AppHandler
  ( handleArticle
  , handleEdit
  ) where

import Control.Monad.CatchIO
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Heist
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
    newArticle _ = pass

    openArticle article
        = isXHR (renderArticle $ V.articleSplices article)
      <|> renderFull (V.articleSplices article)

    renderArticle = renderSplices "article"
    renderFull = renderSplices "base"

handleEdit :: AppHandler ()
handleEdit = do
  title <- textParam "title"
  article <- try $ with repo $ fetchArticle title
  article' <- either (newArticle title) pure article
  let splices = editorSplices $ article' ^. articleTitle
  isXHR (renderEditor splices) <|> renderFull splices
  where
    newArticle title ArticleNotFound = pure $ def & articleTitle .~ title
    newArticle _ _ = pass

    renderEditor = renderSplices "editor"
    renderFull = renderSplices "base"

handleSource :: AppHandler ()
handleSource = isXHR renderSource <|> renderFull where
  renderSource = undefined
  renderFull = undefined

editorSplices :: T.Text -> Splices (I.Splice AppHandler)
editorSplices title = do
  "wiki:title" ## I.textSplice title
  "wiki:content" ## I.callTemplate "editor" noSplices
  "wiki:source" ## I.textSplice "foo"
  "wiki:navigation" ## V.navigation title

isXHR :: AppHandler () -> AppHandler ()
isXHR h = getsRequest (getHeader "X-Requested-With") >>= \case
  Just "XMLHttpRequest" -> h
  _ -> pass

renderSplices :: BS.ByteString -> Splices (I.Splice AppHandler) -> AppHandler ()
renderSplices name splices = heistLocal (I.bindSplices splices) $ render name
