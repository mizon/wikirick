module Wikirick.Backends.JSONConnection
  ( module Wikirick.JSONConnection
  , initJSONConnection
  ) where

import Control.Monad.CatchIO
import Data.Aeson
import Snap

import Wikirick.JSONConnection

initJSONConnection :: SnapletInit b JSONConnection
initJSONConnection = makeSnaplet "JSONConnection" "Provide a JSON connector" Nothing $ do
  return JSONConnection
    { _parseJSON = do
      reqBody <- readRequestBody maxReads
      either throwError return $ eitherDecode reqBody

    , _responseJSON = \v -> do
      modifyResponse $ setContentType "application/json"
      writeLBS $ encode v
    }
  where
    maxReads = 1024 ^ (2 :: Int)  -- 1MB
    throwError = throw . JSONParseError
