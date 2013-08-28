module Wikirick.Site
  ( app
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe

import Wikirick.Application

handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]

handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = loginUser "login" "password" Nothing (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

routes :: [(ByteString, Handler App App ())]
routes =
  [ ("/login", with auth handleLoginSubmit)
  , ("/logout", with auth handleLogout)
  , ("/new_user", with auth handleNewUser)
  , ("", serveDirectory "static")
  ]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
  addRoutes routes
  addAuthSplices h auth
  return $ App h s a
