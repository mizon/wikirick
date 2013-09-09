module Wikirick.View
  ( navigation
  ) where

import qualified Data.Text as T
import Heist
import Heist.Interpreted as I
import Text.XmlHtml

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
