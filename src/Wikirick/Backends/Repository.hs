module Wikirick.Backends.Repository
  ( module Wikirick.Repository
  , initRepository
  , makeRepository
  ) where

import Control.Monad.CatchIO
import Snap
import System.Exit
import qualified System.IO.Streams as S

import Wikirick.Import
import Wikirick.Repository
import Wikirick.Util

initRepository :: FilePath -> SnapletInit b Repository
initRepository = makeSnaplet "repo" "Serves Wiki articles" Nothing . return . makeRepository

makeRepository :: FilePath -> Repository
makeRepository dbDir = Repository
  { _fetchArticle = \title -> liftIO $ do
      (_, out, _, p) <- runInteractiveProcess "co" ["-p", title ^. unpacked]
      source <- consumeText out
      S.getProcessExitCode p >>= \case
        Just ExitSuccess -> return $ def
          & articleTitle .~ title
          & articleSource .~ source
        _ -> throw ArticleNotFound

  , _fetchRevision = undefined

  , _postArticle = \a -> liftIO $ do
      _ <- runInteractiveProcess "co" ["l", a ^. articleTitle . unpacked]
      undefined

  , _fetchAllArticleTitles = undefined
  } where
    runInteractiveProcess cmd opts = do
      (in_, out, err, p) <- S.runInteractiveProcess cmd opts (Just dbDir) Nothing
      in_' <- S.encodeUtf8 in_
      out' <- S.decodeUtf8 out
      err' <- S.decodeUtf8 err
      return (in_', out', err', p)
