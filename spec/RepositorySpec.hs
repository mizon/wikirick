module RepositorySpec
  ( repositorySpec
  ) where

import Control.Exception
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Lens
import qualified System.Directory as D
import System.FilePath
import Test.Hspec

import Wikirick.Backends.Repository
import Wikirick.Import

hasSameContent :: Article -> Article -> Expectation
hasSameContent a a' = do
  a ^. articleTitle `shouldBe` a' ^. articleTitle
  a ^. articleSource `shouldBe` a' ^. articleSource

infix 1 `hasSameContent`

repositorySpec :: Spec
repositorySpec = describe "article repository" $
  around setupDir $ do
    it "saves an article" $ do
      let article = makeArticle "SomePage" "Hello"
      runRepo $ postArticle article

      article' <- runRepo $ fetchArticle "SomePage"
      article' `hasSameContent` article

      today <- Time.zonedTimeToUTC <$> Time.getZonedTime
      article' ^? editDate' . year `shouldBe` Just (today ^. year)
      article' ^? editDate' . month `shouldBe` Just (today ^. month)
      article' ^? editDate' . day `shouldBe` Just (today ^. day)

    it "fetchs articles" $ do
      let pageOne = makeArticle "PageOne" "Hello"
          pageTwo = makeArticle "PageTwo" "Bye"
      runRepo $ do
        postArticle pageOne
        postArticle pageTwo
      pageOne' <- runRepo $ fetchArticle "PageOne"
      pageTwo' <- runRepo $ fetchArticle "PageTwo"
      pageOne' `hasSameContent` pageOne
      pageTwo' `hasSameContent` pageTwo

    it "increments revisions of saved articles" $ do
      let article = makeArticle "SomePage" "Hello"

      runRepo $ postArticle article
      articleRev1 <- runRepo $ fetchArticle "SomePage"
      articleRev1 ^. articleRevision `shouldBe` Just 1

      runRepo $ postArticle $ article & articleSource .~ "Bye"
      articleRev2 <- runRepo $ fetchArticle "SomePage"
      articleRev2 ^. articleRevision `shouldBe` Just 2

    it "fetches an article which has specified revison" $ do
      let rev1 = makeArticle "SomePage" "Hello"
          rev2 = makeArticle "SomePage" "Bye"
      runRepo $ do
        postArticle rev1
        postArticle rev2
      rev1' <- runRepo $ fetchRevision "SomePage" 1
      rev2' <- runRepo $ fetchRevision "SomePage" 2
      rev1' `hasSameContent` rev1
      rev2' `hasSameContent` rev2

    it "fails to fetch invalid revision" $ do
      runRepo $ postArticle $ makeArticle "SomePage" "Hello"
      runRepo (fetchRevision "SomePage" 0) `shouldThrow` (== InvalidRevision)
      runRepo (fetchRevision "SomePage" (-5)) `shouldThrow` (== InvalidRevision)

    it "fails to fetch non existed files" $ do
      runRepo (fetchArticle "SomePage") `shouldThrow` (== ArticleNotFound)
      runRepo (fetchRevision "SomePage" 1) `shouldThrow` (== ArticleNotFound)

editDate' :: Traversal' Article Time.UTCTime
editDate' = editLog . _Just . editDate

makeArticle :: T.Text -> T.Text -> Article
makeArticle t src = def & articleTitle .~ t & articleSource .~ src

repositoryDir :: FilePath
repositoryDir = "testrepo"

runRepo :: MonadIO m => StateT Repository m a -> m a
runRepo s = do
  repo <- liftIO $ makeRepository repositoryDir
  evalStateT s repo

setupDir :: Expectation -> Expectation
setupDir e = do
  D.createDirectory repositoryDir
  D.createDirectory $ repositoryDir </> "RCS"
  e `finally` D.removeDirectoryRecursive repositoryDir
