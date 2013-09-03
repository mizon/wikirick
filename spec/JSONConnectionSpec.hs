module JSONConnectionSpec
  ( jsonConnectionSpec
  ) where

import Data.Aeson hiding (parseJSON)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Snap hiding (get)
import Snap.Snaplet.Test
import Snap.Test hiding (evalHandler, runHandler)
import qualified Snap.Types.Headers as H
import Test.Hspec

import Wikirick.Backends.JSONConnection

evalJSONConnection :: MonadIO m => RequestBuilder m () -> Handler JSONConnection JSONConnection a -> m (Either T.Text a)
evalJSONConnection req action = evalHandler req action initJSONConnection

runJSONConnection :: MonadIO m => RequestBuilder m () -> Handler JSONConnection JSONConnection a -> m (Either T.Text Response)
runJSONConnection req action = runHandler req action initJSONConnection

jsonConnectionSpec :: Spec
jsonConnectionSpec = describe "JSON Connector" $ do
  it "reads JSON requests" $ do
    let value = object ["key" .= ("body" :: String)]
    evalJSONConnection (postJSON value) parseJSON `shouldReturn` Right value

  it "makes JSON responses" $ do
    let value = object ["foo" .= ("foo" :: String)]
    Right res <- runJSONConnection nullRequest $ responseJSON value
    body <- getResponseBody res
    H.lookup "Content-Type" (headers res) `shouldBe` Just ["application/json"]
    decode (L.fromStrict body) `shouldBe` Just value
  where
    postJSON = postRaw "/" "application/json" . L.toStrict . encode
    nullRequest = return ()
