module Wikirick.Backends.Article
  ( module Wikirick.Article
  , initArticleRepository
  ) where

import Control.Exception hiding (try, throw)
import Control.Monad.CatchIO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Snap
import System.FilePath
import System.IO.Streams
import qualified System.IO.Streams.File as F
import qualified System.IO.Streams.List as SL
import qualified System.IO.Streams.Text as ST
import Text.XmlHtml

import Wikirick.Article
import Wikirick.Import

initArticleRepository :: FilePath -> SnapletInit b ArticleRepository
initArticleRepository dbDir = makeSnaplet "ArticleRepository" "Serves Wiki articles" Nothing $ do
  return ArticleRepository
    { _fetchArticle = \title -> do
        source <- liftIO $ try $ TIO.readFile $ sourcePath title
        case source of
          Left (SomeException _) -> throw ArticleNotFound
          Right source'-> return $ def
            & articleTitle .~ title
            & articleSource .~ source'

    , _fetchRevision = \title rev ->
        undefined

    , _postArticle = \a ->
        undefined

    , _fetchAllArticleTitles = undefined
    }
  where
    sourcePath title = dbDir </> title ^. unpacked
