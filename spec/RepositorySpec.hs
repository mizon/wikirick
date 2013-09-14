module RepositorySpec
  ( repositorySpec
  ) where

import Control.Exception
import qualified System.Directory as D
import System.FilePath
import Test.Hspec

import Wikirick.Backends.Repository
import Wikirick.Import

hasSameContent :: Article -> Article -> Expectation
hasSameContent a a' = do
  a ^. articleTitle `shouldBe` a' ^. articleTitle
  a ^. articleSource `shouldBe` a' ^. articleSource

repositorySpec :: Spec
repositorySpec = describe "article repository" $
  around setupDir $ do
    it "saves an article" $ do
      let article = Article "SomePage" "Hello" Nothing
      runRepo $ postArticle article
      article' <- runRepo $ fetchArticle "SomePage"
      article' `hasSameContent` article

    it "fetchs an article" $ do
      let pageOne = Article "PageOne" "Hello" Nothing
          pageTwo = Article "PageTwo" "Bye" Nothing
      runRepo $ do
        postArticle pageOne
        postArticle pageTwo
      pageOne' <- runRepo $ fetchArticle "PageOne"
      pageTwo' <- runRepo $ fetchArticle "PageTwo"
      pageOne' `hasSameContent` pageOne
      pageTwo' `hasSameContent` pageTwo

    it "increments revisions of saved articles" $ do
      let article = Article "SomePage" "Hello" Nothing

      runRepo $ postArticle article
      articleRev1 <- runRepo $ fetchArticle "SomePage"
      articleRev1 ^. articleRevision `shouldBe` Just 1

      runRepo $ postArticle $ article & articleSource .~ "Bye"
      articleRev2 <- runRepo $ fetchArticle "SomePage"
      articleRev2 ^. articleRevision `shouldBe` Just 2

    it "fails to fetch non existed files" $ do
      runRepo (fetchArticle "SomePage") `shouldThrow` (== ArticleNotFound)

repositoryDir :: FilePath
repositoryDir = "testrepo"

runRepo :: Monad m => StateT Repository m a -> m a
runRepo = flip evalStateT repository where
  repository = makeRepository repositoryDir

setupDir :: Expectation -> Expectation
setupDir e = do
  D.createDirectory repositoryDir
  D.createDirectory $ repositoryDir </> "RCS"
  e `finally` D.removeDirectoryRecursive repositoryDir
