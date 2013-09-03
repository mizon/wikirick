module Wikirick.Backends.JSONConnection
  ( module Wikirick.JSONConnection
  , initJSONConnection
  ) where

import Data.Aeson
import Snap

import Wikirick.JSONConnection

initJSONConnection :: SnapletInit b JSONConnection
initJSONConnection = makeSnaplet "JSONConnection" "Provide a JSON connector" Nothing $ do
  return JSONConnection
    { _responseJSON = \v -> do
        modifyResponse $ setContentType "application/json"
        writeLBS $ encode v
    }
