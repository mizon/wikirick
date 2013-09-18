module AppHandlerSpec
  ( appHandlerSpec
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Heist
import qualified Heist.Interpreted as I
import Snap
import qualified Snap.Snaplet.Heist as SH
import qualified Snap.Snaplet.Test as SST
import qualified Snap.Test as ST
import qualified System.Directory as D
import Test.Hspec

import Wikirick.Application
import qualified Wikirick.Backends.Repository as R
import Wikirick.Import
import qualified Wikirick.Site as WS
import qualified Wikirick.View as V

import SpecHelper

runAppHandler :: MonadIO m => ST.RequestBuilder m () -> SnapletInit App App -> m (Either T.Text Response)
runAppHandler req app = SST.runHandler Nothing req (route WS.routes) app

shouldBeRendered :: Response' -> (BS.ByteString, Splices (I.Splice AppHandler)) -> Expectation
shouldBeRendered res (templateName, s) = do
  res' <- mustBeSuccess res
  body <- ST.getResponseBody res'
  body' <- renderView
  body `shouldBe` body'
  where
    renderView = do
      res' <- SST.runHandler Nothing (return ()) doRender $ mockedApp repositoryMock
      res'' <- mustBeSuccess res'
      liftIO $ ST.getResponseBody res''

    doRender = SH.renderWithSplices templateName s

appHandlerSpec :: Spec
appHandlerSpec = describe "main handler" $
  after (D.removeFile "site_key_test.txt") $ do
    it "opens article pages" $ do
      let article = def & R.articleTitle .~ "FrontPage"
      let repo' = repositoryMock
            { R._fetchArticle = \title -> case title of
                "FrontPage" -> return article
                _ -> fail "invalid call"
            }
      res <- runAppHandler (ST.get "/wiki/FrontPage" mempty) $ mockedApp repo'
      res `shouldHaveStatus` 200
      res `shouldBeRendered` ("base", V.articleSplices article)

    it "opens article pages fragment" $ do
      let article = def
      let repo' = repositoryMock
            { R._fetchArticle = \title -> case title of
                "FrontPage" -> return article
                _ -> fail "invalid call"
            }
      res <- runAppHandler (xhr >> ST.get "/wiki/FrontPage" mempty) $ mockedApp repo'
      res `shouldHaveStatus` 200
      res `shouldBeRendered` ("article", V.articleSplices article)
