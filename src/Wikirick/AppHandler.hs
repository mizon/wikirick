module Wikirick.AppHandler
  ( handleArticle
  , handleEdit
  ) where

import Data.Aeson hiding (json)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import Snap
import Snap.Snaplet.Heist
import Text.XmlHtml hiding (render)

import Wikirick.Application
import Wikirick.Article
import Wikirick.Import hiding ((.=))
import Wikirick.JSONConnection
import Wikirick.Util

handleArticle :: AppHandler ()
handleArticle = isXHR renderArticle <|> renderFull where
  renderArticle = renderSplices "article" articleSplices
  renderFull = renderSplices "index" articleSplices

articleSplices :: [(T.Text, I.Splice AppHandler)]
articleSplices =
  [ ("wiki:title", I.textSplice "FrontPage")
  , ("wiki:content", I.callTemplate "article" [])
  , ("wiki:sections", sections)
  ] where
  sections = pure
    [ Element "section" []
      [ Element "h2" [] [TextNode "What is this?"]
      , Element "p" [] [TextNode "Haskell"]
      ]
    ]

handleEdit :: AppHandler ()
handleEdit = isXHR renderEditor <|> renderFull where
  renderEditor = renderSplices "editor" editorSplices
  renderFull = renderSplices "index" editorSplices

handleSource :: AppHandler ()
handleSource = isXHR renderSource <|> renderFull where
  renderSource = undefined
  renderFull = undefined

editorSplices :: [(T.Text, I.Splice AppHandler)]
editorSplices =
  [ ("wiki:title", I.textSplice "FrontPage")
  , ("wiki:content", I.callTemplate "editor" [])
  , ("wiki:source", I.textSplice "foo")
  ]

isXHR :: AppHandler () -> AppHandler ()
isXHR h = getsRequest (getHeader "X-Requested-With") >>= \case
  Just "XMLHttpRequest" -> h
  _ -> pass

renderSplices :: BS.ByteString -> [(T.Text, I.Splice AppHandler)] -> AppHandler ()
renderSplices name splices = heistLocal (I.bindSplices splices) $ render name
