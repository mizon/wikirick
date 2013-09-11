module SpecMain where

import Test.Hspec

import JSONConnectionSpec
import URLMapperSpec

main :: IO ()
main = hspec $ do
  jsonConnectionSpec
  urlMapperSpec
