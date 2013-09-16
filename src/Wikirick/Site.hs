module Wikirick.Site
  ( routes
  , app
  ) where

import Data.ByteString (ByteString)
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe

import Wikirick.AppHandler
import Wikirick.Application
import qualified Wikirick.Backends.JSONConnection as J
import qualified Wikirick.Backends.Repository as R
import qualified Wikirick.Backends.URLMapper as U

routes :: [(ByteString, AppHandler ())]
routes =
  [ ("/wiki/", handleArticle)
  , ("/wiki/:title", handleArticle)
  , ("/wiki/:title/edit", handleEdit)
  , ("/js", serveDirectory "static/js")
  , ("/css", serveDirectory "static/css")
  , ("", pass)
  ]

app :: SnapletInit App App
app = makeSnaplet "app" "The main snaplet of this application" Nothing $ do
  addRoutes routes
  r <- liftIO $ R.makeRepository "repo"
  App
    <$> nestSnaplet "" heist (heistInit "templates")
    <*> nestSnaplet "" sess (initCookieSessionManager "site_key.txt" "sess" $ Just 3600)
    <*> nestSnaplet "" json J.initJSONConnection
    <*> nestSnaplet "" repo (R.initRepository r)
    <*> nestSnaplet "" urlReceiver (U.initURLReceiver $ U.initURLMapper "/")
