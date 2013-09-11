module Wikirick.Article where

import Control.Exception hiding (Handler)
import Control.Monad.CatchIO
import qualified Data.Text as T
import Data.Typeable
import Snap

import Wikirick.Import

data Article = Article
  { _articleTitle :: T.Text
  , _articleSource :: T.Text
  , _articleRevision :: Maybe Integer
  } deriving (Show, Eq)
makeLenses ''Article

instance Default Article where
  def = Article "" "" Nothing

data ArticleRepository = ArticleRepository
  { _fetchArticle :: MonadCatchIO m => T.Text -> m Article
  , _fetchRevision :: MonadCatchIO m => T.Text -> Integer -> m Article
  , _postArticle :: MonadCatchIO m => Article -> m ()
  , _fetchAllArticleTitles :: MonadCatchIO m => m [T.Text]
  }

fetchArticle :: (MonadState ArticleRepository m, MonadCatchIO m) => T.Text -> m Article
fetchArticle t = get >>= \self ->
  _fetchArticle self t

fetchRevision :: (MonadState ArticleRepository m, MonadCatchIO m) => T.Text -> Integer -> m Article
fetchRevision t rev = get >>= \self ->
  _fetchRevision self t rev

postArticle :: (MonadState ArticleRepository m, MonadCatchIO m) => Article -> m ()
postArticle a = get >>= \self ->
  _postArticle self a

fetchAllArticleTitles :: (MonadState ArticleRepository m, MonadCatchIO m) => m [T.Text]
fetchAllArticleTitles = get >>= _fetchAllArticleTitles

data ArticleNotFound = ArticleNotFound deriving (Typeable, Show)
instance Exception ArticleNotFound
