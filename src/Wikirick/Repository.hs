module Wikirick.Repository where

import Control.Exception hiding (Handler)
import Control.Monad.CatchIO
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Typeable
import Snap

import Wikirick.Import

data EditLog = EditLog
  { _editDate :: Time.UTCTime
  , _editComment :: T.Text
  } deriving (Show, Eq)
makeLenses ''EditLog

data Article = Article
  { _articleTitle :: T.Text
  , _articleSource :: T.Text
  , _articleRevision :: Maybe Integer
  , _editLog :: Maybe EditLog
  } deriving (Show, Eq)
makeLenses ''Article

instance Default Article where
  def = Article "" "" Nothing Nothing

data Repository = Repository
  { _fetchArticle :: MonadCatchIO m => T.Text -> m Article
  , _fetchRevision :: MonadCatchIO m => T.Text -> Integer -> m Article
  , _postArticle :: MonadCatchIO m => Article -> m ()
  , _fetchAllArticleTitles :: MonadCatchIO m => m [T.Text]
  }

fetchArticle :: (MonadState Repository m, MonadCatchIO m) => T.Text -> m Article
fetchArticle t = get >>= \self ->
  _fetchArticle self t

fetchRevision :: (MonadState Repository m, MonadCatchIO m) => T.Text -> Integer -> m Article
fetchRevision t rev = get >>= \self ->
  _fetchRevision self t rev

postArticle :: (MonadState Repository m, MonadCatchIO m) => Article -> m ()
postArticle a = get >>= \self ->
  _postArticle self a

fetchAllArticleTitles :: (MonadState Repository m, MonadCatchIO m) => m [T.Text]
fetchAllArticleTitles = get >>= _fetchAllArticleTitles

data RepositoryException
  = ArticleNotFound
  | InvalidRevision
  | RepositoryException BS.ByteString deriving
    ( Eq
    , Show
    , Typeable
    )
instance Exception RepositoryException
