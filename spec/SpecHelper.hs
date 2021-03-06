module SpecHelper where

import Data.Foldable
import Data.IORef
import qualified Data.Sequence as S
import qualified Data.Text as T
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import qualified Snap.Test as ST
import Test.Hspec

import Wikirick.Application
import qualified Wikirick.Backends.JSONConnection as J
import qualified Wikirick.Backends.Repository as R
import qualified Wikirick.Backends.URLMapper as U
import Wikirick.Import
import Wikirick.JSONConnection
import Wikirick.Repository

mustNotBeCalled :: String -> a
mustNotBeCalled name = error $ "must not be called: " <> name

repositoryMock :: Repository
repositoryMock = Repository
  { _fetchArticle = mustNotBeCalled "fetchArticle"
  , _fetchRevision = mustNotBeCalled "fetchRevision"
  , _postArticle = mustNotBeCalled "postArticle"
  , _fetchAllArticleTitles = mustNotBeCalled "fetchAllArticleTitles"
  }

jsonConnectionMock :: JSONConnection
jsonConnectionMock = JSONConnection
  { _parseJSON = mustNotBeCalled "parseJSON"
  , _responseJSON = mustNotBeCalled "responseJSON"
  }

mockedApp :: R.Repository -> SnapletInit App App
mockedApp repo' = makeSnaplet "" "" Nothing $ App
  <$> nestSnaplet "" heist (heistInit "templates")
  <*> nestSnaplet "" sess (initCookieSessionManager "site_key_test.txt" "sess" Nothing)
  <*> nestSnaplet "" json J.initJSONConnection
  <*> nestSnaplet "" repo (R.initRepository repo')
  <*> nestSnaplet "" urlReceiver (U.initURLReceiver $ U.initURLMapper "/")

data Spy a = Spy
  { logValue :: MonadIO m => a -> m ()
  , mustBeLogged :: (Show a, Eq a) => [a] -> Expectation
  }

newSpy :: IO (Spy a)
newSpy = do
  values <- newIORef S.empty
  return Spy
    { logValue = \v ->
        liftIO $ modifyIORef values (|> v)

    , mustBeLogged = \expectations -> do
        vs <- readIORef values
        toList vs `shouldBe` expectations
    }

type Response' = Either T.Text Response

mustBeRight :: MonadIO m => Response' -> m Response
mustBeRight = liftIO . either (fail . T.unpack) pure

shouldHaveStatus :: Response' -> Int -> Expectation
shouldHaveStatus res st = do
  res' <- mustBeRight res
  rspStatus res' `shouldBe` st

xhr :: Monad m => ST.RequestBuilder m ()
xhr = ST.addHeader "X-Requested-With" "XMLHttpRequest"
