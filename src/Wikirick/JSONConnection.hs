module Wikirick.JSONConnection where

import Data.Aeson
import Snap

import Wikirick.Import

class HasJSONConnection m where
  getJSONConnection :: m JSONConnection

instance HasJSONConnection (Handler b JSONConnection) where
  getJSONConnection = get

data JSONConnection = JSONConnection
  { _responseJSON :: (MonadSnap m, ToJSON a) => a -> m ()
  }

responseJSON :: (HasJSONConnection m, MonadSnap m, ToJSON a) => a -> m ()
responseJSON v = getJSONConnection >>= \self ->
  _responseJSON self v
