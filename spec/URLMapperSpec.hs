module URLMapperSpec where

import Test.Hspec

import Wikirick.Article
import Wikirick.Backends.URLMapper
import Wikirick.Import

urlMapper :: URLMapper
urlMapper = initURLMapper "/base"

shouldExpand :: URL -> String -> Expectation
shouldExpand url expectation = expandURL urlMapper url `shouldBe` expectation

urlMapperSpec :: Spec
urlMapperSpec = describe "URL mapper" $ do
  it "expands article pathes" $ do
    ArticlePath (def & articleTitle .~ "foo") `shouldExpand` "/base/wiki/foo"

  it "expands edit pathes" $ do
    EditPath (def & articleTitle .~ "bar") `shouldExpand` "/base/wiki/bar/edit"
