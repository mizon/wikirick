module Wikirick.Util
  ( text
  , textParam
  , consumeText
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding
import Snap
import System.IO.Streams hiding (decodeUtf8)
import qualified System.IO.Streams.List as SL

text :: T.Text -> T.Text
text = id

textParam :: MonadSnap m => BS.ByteString -> m T.Text
textParam name = do
  param <- getParam name
  maybe pass (pure . decodeUtf8) param

consumeText :: InputStream T.Text -> IO T.Text
consumeText os = T.concat <$> SL.toList os
