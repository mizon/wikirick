module SpecMain where

import Test.Hspec

import JSONConnectionSpec

main :: IO ()
main = hspec jsonConnectionSpec
