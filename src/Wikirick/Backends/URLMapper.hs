module Wikirick.Backends.URLMapper
  ( module Wikirick.URLMapper
  , initURLMapper
  ) where

import qualified Data.ByteString.Lens as BL
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import Snap
import System.FilePath

import Wikirick.Article
import Wikirick.Import
import Wikirick.URLMapper

initURLMapper :: String -> SnapletInit a URLMapper
initURLMapper basePath = makeSnaplet "urlmapper" "Provide a URL mapper" Nothing $ do
  return URLMapper
    { _urlSplice = \url ->
        I.textSplice $ expandURL url ^. packed

    , _redirectTo = \url ->
        redirect $ expandURL url ^. BL.packedChars
    }
  where
    expandURL url = basePath </> case url of
      ArticlePath a -> "wiki" </> a ^. articleTitle . unpacked
      EditPath a -> "wiki" </> a ^. articleTitle . unpacked </> "edit"
