module Wikirick.View
  ( articleSplices
  , navigation
  ) where

import qualified Data.Text as T
import Heist
import Heist.Interpreted as I
import Text.XmlHtml

import Wikirick.Application
import Wikirick.Import
import Wikirick.Repository

articleSplices :: Article -> Splices (I.Splice AppHandler)
articleSplices a = do
  "wiki:title" ## I.textSplice $ a ^. articleTitle
  "wiki:content" ## I.callTemplate "article" noSplices
  "wiki:sections" ## sections
  "wiki:navigation" ## navigation $ a ^. articleTitle
  where
    sections = pure
      [ Element "section" []
        [ TextNode $ a ^. articleSource
        ]
      ]

editor :: Monad m => Article -> Splice m
editor a = I.callTemplate "editor" $ do
  "wiki:title" ## I.textSplice $ a ^. articleTitle
  "wiki:content" ##  I.callTemplate "editor" noSplices
  "wiki:source" ## I.textSplice "foo"
  "wiki:navigation" ## navigation $ a ^. articleTitle

navigation :: Monad m => T.Text -> Splice m
navigation title = return
  [ Element "nav" [("id", "navigation")]
    [ Element "ul" [] $ pageTitle : navItems
    ]
  ] where
    pageTitle = Element "li" [("class", "title")] [TextNode title]

    navItems = navItem =<<
      [ "Article"
      , "Edit"
      , "Source"
      , "History"
      ]

navItem :: T.Text -> Template
navItem name =
  [ Element "li" []
    [ Element "a" []
      [ TextNode name
      ]
    ]
  ]
