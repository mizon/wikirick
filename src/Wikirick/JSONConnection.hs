module Wikirick.JSONConnection where

import Control.Exception hiding (Handler)
import Data.Aeson
import Data.Typeable
import Snap

class HasJSONConnection m where
  getJSONConnection :: m JSONConnection

instance HasJSONConnection (Handler b JSONConnection) where
  getJSONConnection = get

data JSONParseError = JSONParseError String
  deriving (Show, Typeable)
instance Exception JSONParseError

data JSONConnection = JSONConnection
  { _parseJSON :: (MonadSnap m, FromJSON a) => m a
  , _responseJSON :: (MonadSnap m, ToJSON a) => a -> m ()
  }

parseJSON :: (HasJSONConnection m, MonadSnap m, FromJSON a) => m a
parseJSON = getJSONConnection >>= _parseJSON

responseJSON :: (HasJSONConnection m, MonadSnap m, ToJSON a) => a -> m ()
responseJSON v = getJSONConnection >>= \self ->
  _responseJSON self v
