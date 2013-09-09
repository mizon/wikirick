module Wikirick.Util
  ( text
  , textParam
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Snap
import Data.Text.Encoding

text :: T.Text -> T.Text
text = id

textParam :: MonadSnap m => BS.ByteString -> m T.Text
textParam name = do
  param <- getParam name
  maybe pass (pure . decodeUtf8) param
