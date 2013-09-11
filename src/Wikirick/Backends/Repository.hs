module Wikirick.Backends.Repository
  ( module Wikirick.Repository
  , initRepository
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

import Wikirick.Repository
import Wikirick.Import

initRepository :: FilePath -> SnapletInit b Repository
initRepository dbDir = makeSnaplet "repo" "Serves Wiki articles" Nothing $ do
  return Repository
    { _fetchArticle = \title -> do
        source <- liftIO $ try $ TIO.readFile $ sourcePath title
        case source of
          Left (SomeException _) -> throw ArticleNotFound
          Right source' -> return $ def
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
