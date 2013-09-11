module Wikirick.Backends.URLMapper
  ( module Wikirick.URLMapper
  , initURLMapper
  , initURLReceiver
  ) where

import qualified Data.ByteString.Lens as BL
import qualified Heist.Interpreted as I
import Snap
import System.FilePath

import Wikirick.Article
import Wikirick.Import
import Wikirick.URLMapper

initURLMapper :: String -> URLMapper
initURLMapper basePath = URLMapper $ \url ->
  basePath </> case url of
    ArticlePath a -> "wiki" </> a ^. articleTitle . unpacked
    EditPath a -> "wiki" </> a ^. articleTitle . unpacked </> "edit"

initURLReceiver :: URLMapper -> SnapletInit a URLReceiver
initURLReceiver mapper = makeSnaplet "urlmapper" "Provide a URL mapper" Nothing $ do
  return URLReceiver
    { _urlSplice = \url ->
        I.textSplice $ expandURL mapper url ^. packed

    , _redirectTo = \url ->
        redirect $ expandURL mapper url ^. BL.packedChars
    }
