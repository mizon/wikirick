module URLMapperSpec where

import Test.Hspec

import Wikirick.Backends.URLMapper
import Wikirick.Import
import Wikirick.Repository

urlMapper :: URLMapper
urlMapper = initURLMapper "/base"

beExpanded :: URL -> String -> Expectation
beExpanded url expectation = expandURL urlMapper url `shouldBe` expectation

urlMapperSpec :: Spec
urlMapperSpec = describe "URL mapper" $ do
  it "expands article pathes" $ do
    ArticlePath (def & articleTitle .~ "foo") `beExpanded` "/base/wiki/foo"

  it "expands edit pathes" $ do
    EditPath (def & articleTitle .~ "bar") `beExpanded` "/base/wiki/bar/edit"
