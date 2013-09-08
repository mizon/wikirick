module Wikirick.Site
  ( app
  ) where

import Data.ByteString (ByteString)
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe

import Wikirick.AppHandler
import Wikirick.Application
import qualified Wikirick.Backends.JSONConnection as J

routes :: [(ByteString, AppHandler ())]
routes =
  [ ("wiki/", handleArticle)
  , ("wiki/:title", handleArticle)
  , ("wiki/:title/edit", handleEdit)
  , ("/js", serveDirectory "static/js")
  , ("/css", serveDirectory "static/css")
  ]

app :: SnapletInit App App
app = makeSnaplet "app" "The main snaplet of this application" Nothing $ do
  addRoutes routes
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "" sess $ initCookieSessionManager "site_key.txt" "sess" $ Just 3600
  a <- nestSnaplet "" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
  j <- nestSnaplet "" json J.initJSONConnection
  return $ App h s a j
