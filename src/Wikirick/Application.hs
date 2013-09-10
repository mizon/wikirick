module Wikirick.Application where

import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

import Wikirick.Article
import Wikirick.JSONConnection
import qualified Wikirick.URLMapper as U

data App = App
  { _heist :: Snaplet (Heist App)
  , _sess :: Snaplet SessionManager
  , _auth :: Snaplet (AuthManager App)
  , _json :: Snaplet JSONConnection
  , _articles :: Snaplet (ArticleRepository App)
  , _urlMapper :: Snaplet U.URLMapper
  }
makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance U.HasURLMapper App where
  refURLMapper = urlMapper

type AppHandler = Handler App App
