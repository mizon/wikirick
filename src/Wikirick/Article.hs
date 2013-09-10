module Wikirick.Article where

import Control.Exception hiding (Handler)
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

type ARHandler b = Handler b (ArticleRepository b)

data ArticleRepository b = ArticleRepository
  { _fetchArticle :: T.Text -> ARHandler b Article
  , _fetchRevision :: T.Text -> Integer -> ARHandler b Article
  , _postArticle :: Article -> ARHandler b ()
  , _fetchAllArticleTitles :: ARHandler b [T.Text]
  }

fetchArticle :: T.Text -> ARHandler b Article
fetchArticle t = get >>= \self ->
  _fetchArticle self t

fetchRevision :: T.Text -> Integer -> ARHandler b Article
fetchRevision t rev = get >>= \self ->
  _fetchRevision self t rev

postArticle :: Article -> ARHandler b ()
postArticle a = get >>= \self ->
  _postArticle self a

fetchAllArticleTitles :: ARHandler b [T.Text]
fetchAllArticleTitles = get >>= _fetchAllArticleTitles

data ArticleNotFound = ArticleNotFound deriving (Typeable, Show)
instance Exception ArticleNotFound
