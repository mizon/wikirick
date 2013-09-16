module SpecMain where

import Test.Hspec

import AppHandlerSpec
import JSONConnectionSpec
import RepositorySpec
import URLMapperSpec

main :: IO ()
main = hspec $ do
  appHandlerSpec
  jsonConnectionSpec
  repositorySpec
  urlMapperSpec
