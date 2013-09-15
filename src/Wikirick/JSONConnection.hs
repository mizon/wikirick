module Wikirick.JSONConnection where

import Control.Exception hiding (Handler)
import Data.Aeson
import Data.Typeable
import Snap

data JSONParseError = JSONParseError String deriving (Eq, Show, Typeable)
instance Exception JSONParseError

data JSONConnection = JSONConnection
  { _parseJSON :: (MonadSnap m, FromJSON a) => m a
  , _responseJSON :: (MonadSnap m, ToJSON a) => a -> m ()
  }

parseJSON :: (MonadState JSONConnection m, MonadSnap m, FromJSON a) => m a
parseJSON = get >>= _parseJSON

responseJSON :: (MonadState JSONConnection m, MonadSnap m, ToJSON a) => a -> m ()
responseJSON v = get >>= \self ->
  _responseJSON self v
