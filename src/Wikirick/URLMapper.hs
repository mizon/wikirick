module Wikirick.URLMapper where

import Heist
import Heist.Interpreted
import Snap

import Wikirick.Article
import Wikirick.Import

data URL
  = ArticlePath Article
  | EditPath Article

newtype URLMapper = URLMapper
  { expandURL :: URL -> String
  }

class HasURLReceiver a where
  refURLReceiver :: Getter a URLReceiver

data URLReceiver = URLReceiver
  { _urlSplice :: Monad m => URL -> Splice m
  , _redirectTo :: MonadSnap m => URL -> m a
  }

urlSplice :: (HasURLReceiver s, MonadState s m) => URL -> Splice m
urlSplice u = use refURLReceiver >>= \self ->
  _urlSplice self u

redirectTo :: (MonadSnap m, HasURLReceiver s, MonadState s m) => URL -> m a
redirectTo u = use refURLReceiver >>= \self ->
  _redirectTo self u
