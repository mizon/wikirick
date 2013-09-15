module Wikirick.Backends.Repository
  ( module Wikirick.Repository
  , initRepository
  , makeRepository
  ) where

import Control.Monad.CatchIO
import qualified Data.Attoparsec as A
import qualified Data.ByteString as BS
import qualified Data.Char as C
import Data.Word
import Snap
import System.Exit
import System.FilePath
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as SA

import Wikirick.Import
import Wikirick.Repository
import Wikirick.Util

initRepository :: Repository -> SnapletInit b Repository
initRepository = makeSnaplet "repo" "Serves Wiki articles" Nothing . return

makeRepository :: FilePath -> Repository
makeRepository dbDir = Repository
  { _fetchArticle = \title ->
      fetchArticle' title ["-p", title ^. unpacked]

  , _fetchRevision = \title rev -> do
      when (rev < 1) $ throw InvalidRevision
      fetchArticle' title ["-r1." <> show rev, "-p", title ^. unpacked]

  , _postArticle = \a -> liftIO $ do
      checkOutRCSFile a
      S.withFileAsOutput (articlePath a) $ \out -> do
        textOut <- S.encodeUtf8 out
        S.write (Just $ a ^. articleSource) textOut

      (in_, _, err, p) <- runInteractiveProcess "ci" [a ^. articleTitle . unpacked]
      S.write Nothing in_
      S.waitForProcess p >>= \case
        ExitSuccess -> return ()
        _ -> throwFromRCSError err

  , _fetchAllArticleTitles = undefined
  } where
    fetchArticle' title coOptions = liftIO $ do
      (_, out, err, p) <- runInteractiveProcess "co" coOptions
      source <- consumeText =<< S.decodeUtf8 out
      S.waitForProcess p >>= \case
        ExitSuccess -> do
          rev <- SA.parseFromStream revParser err
          return $ def
            & articleTitle .~ title
            & articleSource .~ source
            & articleRevision .~ Just rev
        _ -> throwFromRCSError err

    checkOutRCSFile article = do
      (_, _, err, p) <- runInteractiveProcess "co" ["-l", article ^. articleTitle . unpacked]
      S.waitForProcess p >>= \case
        ExitSuccess -> return ()
        _ -> throwFromRCSError err `catch` \case
          ArticleNotFound -> return ()
          e -> throw e

    articlePath a = dbDir </> a ^. articleTitle . unpacked
    runInteractiveProcess cmd opts = S.runInteractiveProcess cmd opts (Just dbDir) Nothing

throwFromRCSError :: S.InputStream BS.ByteString -> IO a
throwFromRCSError = throw <=< SA.parseFromStream errorParser

errorParser :: A.Parser RepositoryException
errorParser
    = ArticleNotFound <$ A.try (skipToAfterColon *> skipToAfterColon *> A.string " No such file or directory\n")
  <|> RepositoryException <$> consumeAll where
    consumeAll = BS.pack <$> A.manyTill A.anyWord8 A.endOfInput
    skipToAfterColon = skipTill $ A.word8 $ c2w ':'

revParser :: A.Parser Integer
revParser = do
  skipTill $ A.word8 $ c2w '\n'
  skipTill $ A.word8 $ c2w '.'
  rev <- A.manyTill A.anyWord8 (A.word8 $ c2w '\n')
  return $ read $ w2c <$> rev

w2c :: Word8 -> Char
w2c = C.chr . fromIntegral

c2w :: Char -> Word8
c2w = fromIntegral . C.ord

skipTill :: A.Parser a -> A.Parser ()
skipTill = void . A.manyTill A.anyWord8
