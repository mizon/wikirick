module Wikirick.AppHandler
  ( handleArticle
  , handleEdit
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import Snap
import Snap.Snaplet.Heist
import Text.XmlHtml hiding (render)

import Wikirick.Application
import Wikirick.Util
import qualified Wikirick.View as V

handleArticle :: AppHandler ()
handleArticle = do
  splices <- articleSplices <$> textParam "title"
  isXHR (renderArticle splices) <|> renderFull splices
  where
    renderArticle = renderSplices "article"
    renderFull = renderSplices "base"

articleSplices :: T.Text -> [(T.Text, I.Splice AppHandler)]
articleSplices title =
  [ ("wiki:title", I.textSplice "FrontPage")
  , ("wiki:content", I.callTemplate "article" [])
  , ("wiki:sections", sections)
  , ("wiki:navigation", V.navigation title)
  ] where
  sections = pure
    [ Element "section" []
      [ Element "h2" [] [TextNode "What is this?"]
      , Element "p" [] [TextNode "Haskell"]
      ]
    ]

handleEdit :: AppHandler ()
handleEdit = do
  splices <- editorSplices <$> textParam "title"
  isXHR (renderEditor splices) <|> renderFull splices
  where
    renderEditor = renderSplices "editor"
    renderFull = renderSplices "base"

handleSource :: AppHandler ()
handleSource = isXHR renderSource <|> renderFull where
  renderSource = undefined
  renderFull = undefined

editorSplices :: T.Text -> [(T.Text, I.Splice AppHandler)]
editorSplices title =
  [ ("wiki:title", I.textSplice "FrontPage")
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
