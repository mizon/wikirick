module Wikirick.Application where

import Control.Lens
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

import Wikirick.Repository
import Wikirick.JSONConnection
import qualified Wikirick.URLMapper as U

data App = App
  { _heist :: Snaplet (Heist App)
  , _sess :: Snaplet SessionManager
  , _auth :: Snaplet (AuthManager App)
  , _json :: Snaplet JSONConnection
  , _repo :: Snaplet Repository
  , _urlReceiver :: Snaplet U.URLReceiver
  }
makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance U.HasURLReceiver App where
  refURLReceiver = urlReceiver . snapletValue

type AppHandler = Handler App App
