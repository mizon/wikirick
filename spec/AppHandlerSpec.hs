module AppHandlerSpec
  ( appHandlerSpec
  ) where

import Control.Monad.CatchIO
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

beRendered :: Response' -> (BS.ByteString, Splices (I.Splice AppHandler)) -> Expectation
beRendered res (templateName, s) = do
  res' <- mustBeRight res
  body <- ST.getResponseBody res'
  body' <- renderView
  body `shouldBe` body'
  where
    renderView = do
      res' <- SST.runHandler Nothing (return ()) doRender $ mockedApp repositoryMock
      res'' <- mustBeRight res'
      liftIO $ ST.getResponseBody res''

    doRender = SH.renderWithSplices templateName s

appHandlerSpec :: Spec
appHandlerSpec = describe "main handler" $
  after (D.removeFile "site_key_test.txt") $ do
    it "opens article pages" $ do
      let article = def & R.articleTitle .~ "FrontPage"
      let repo' = repositoryMock
            { R._fetchArticle = \case
                "FrontPage" -> return article
                _ -> fail "invalid call"
            }
      res <- runAppHandler (ST.get "/wiki/FrontPage" mempty) $ mockedApp repo'
      res `hasStatus` 200
      res `beRendered` ("base", V.articleSplices article)

    it "opens article page fragments" $ do
      let article = def
      let repo' = repositoryMock
            { R._fetchArticle = \case
                "FrontPage" -> return article
                _ -> fail "invalid call"
            }
      res <- runAppHandler (xhr >> ST.get "/wiki/FrontPage" mempty) $ mockedApp repo'
      res `hasStatus` 200
      res `beRendered` ("article", V.articleSplices article)

    it "redirects to the new article page" $ do
      let repo' = repositoryMock
            { R._fetchArticle = \case
                "SomeNewPage" -> throw R.ArticleNotFound
                _ -> fail "invalid call"
            }
      res <- runAppHandler (ST.get "/wiki/SomeNewPage" mempty) $ mockedApp repo'
      res `beRedirectedTo` "/wiki/SomeNewPage/edit"

    it "opens a 'New' article page" $ do
      let repo' = repositoryMock
            { R._fetchArticle = \case
                "SomeNewPage" -> throw R.ArticleNotFound
                _ -> fail "invalid call"
            }

      res <- runAppHandler (ST.get "/wiki/SomeNewPage/edit" mempty) $ mockedApp repo'
      res `hasStatus` 200
      res `beRendered` ("base", V.editorSplices "SomeNewPage")

      res' <- runAppHandler (xhr >> ST.get "/wiki/SomeNewPage/edit" mempty) $ mockedApp repo'
      res' `hasStatus` 200
      res' `beRendered` ("editor", V.editorSplices "SomeNewPage")

    it "opens an 'Edit' article page" $ do
      let article = def & R.articleTitle .~ "SomePage"
      let repo' = repositoryMock
            { R._fetchArticle = \case
                "SomePage" -> return article
                _ -> fail "invalid call"
            }

      res <- runAppHandler (ST.get "/wiki/SomePage/edit" mempty) $ mockedApp repo'
      res `hasStatus` 200
      res `beRendered` ("base", V.editorSplices "SomePage")

      res' <- runAppHandler (xhr >> ST.get "/wiki/SomePage/edit" mempty) $ mockedApp repo'
      res' `hasStatus` 200
      res' `beRendered` ("editor", V.editorSplices "SomePage")
