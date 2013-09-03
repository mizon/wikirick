module JSONConnectionSpec
  ( jsonConnectionSpec
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Snap
import Snap.Snaplet.Test
import Snap.Test hiding (evalHandler, runHandler)
import qualified Snap.Types.Headers as H
import Test.Hspec

import Wikirick.Backends.JSONConnection

runJSONConnection :: MonadIO m => Handler JSONConnection JSONConnection a -> m (Either T.Text Response)
runJSONConnection action = runHandler (return ()) action initJSONConnection

jsonConnectionSpec :: Spec
jsonConnectionSpec = describe "JSON Connector" $ do
  it "makes JSON responses" $ do
    let value = object ["foo" .= ("foo" :: String)]
    Right res <- runJSONConnection $ responseJSON value
    body <- getResponseBody res
    H.lookup "Content-Type" (headers res) `shouldBe` Just ["application/json"]
    decode (L.fromStrict body) `shouldBe` Just value
