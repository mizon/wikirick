module AppHandlerSpec
  ( appHandlerSpec
  ) where

import qualified Data.Text as T
import Snap
import qualified Snap.Snaplet.Test as SST
import qualified Snap.Test as ST
import Test.Hspec

import Wikirick.Application
import qualified Wikirick.Backends.Repository as R
import Wikirick.Import
import qualified Wikirick.Site as WS

import SpecHelper

runAppHandler :: MonadIO m => ST.RequestBuilder m () -> SnapletInit App App -> m (Either T.Text Response)
runAppHandler req app = SST.runHandler Nothing req (route WS.routes) app

appHandlerSpec :: Spec
appHandlerSpec = describe "main handler" $ do
  it "opens article pages" $ do
    let repo' = repositoryMock
          { R._fetchArticle = \title -> case title of
              "FrontPage" -> return def
              _ -> fail "invalid call"
          }
    res <- runAppHandler (ST.get "/wiki/FrontPage" mempty) $ mockedApp repo'
    res `statusShouldBe` 200

  it "opens article pages fragment" $ do
    let repo' = repositoryMock
          { R._fetchArticle = \title -> case title of
              "FrontPage" -> return def
              _ -> fail "invalid call"
          }
    res <- runAppHandler (ST.get "/wiki/FrontPage" mempty) $ mockedApp repo'
    res `statusShouldBe` 200
