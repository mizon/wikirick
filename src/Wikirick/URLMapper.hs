module Wikirick.URLMapper where

import Heist
import Heist.Interpreted
import Snap

import Wikirick.Article
import Wikirick.Import

class HasURLMapper a where
  refURLMapper :: Getter a (Snaplet URLMapper)

data URL
  = ArticlePath Article
  | EditPath Article

data URLMapper = URLMapper
  { _urlSplice :: Monad m => URL -> Splice m
  , _redirectTo :: MonadSnap m => URL -> m a
  }

urlSplice :: (HasURLMapper s, MonadState s m) => URL -> Splice m
urlSplice u = use (refURLMapper . snapletValue) >>= \self ->
  _urlSplice self u

redirectTo :: (MonadSnap m, HasURLMapper s, MonadState s m) => URL -> m a
redirectTo u = use (refURLMapper . snapletValue) >>= \self ->
  _redirectTo self u
