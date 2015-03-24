--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
-- http://www.haskell.org/haskellwiki/HUnit_1.0_User%27s_Guide
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM, when)
import qualified Data.ByteString.Lazy.Char8 as B
import Network.API.TheMovieDB
import Network.API.TheMovieDB.Internal.Types
import Network.API.TheMovieDB.Internal.TheMovieDB
import System.Exit (exitFailure)
import Data.List (isPrefixOf)
import Data.Time (fromGregorian)

--------------------------------------------------------------------------------
import Test.HUnit
  (Test(..), Counts(..),
   runTestTT, assertFailure,
   assertEqual, assertBool)

--------------------------------------------------------------------------------
loadGoodMovieFile :: RequestFunction
loadGoodMovieFile _ _ = liftIO $
  liftM Right $ fmap B.pack (readFile "test/movie-good.json")

--------------------------------------------------------------------------------
loadBadMovieFile :: RequestFunction
loadBadMovieFile _ _ = liftIO $
  liftM Right $ fmap B.pack (readFile "test/movie-bad.json")

--------------------------------------------------------------------------------
fakeNetworkError :: RequestFunction
fakeNetworkError _ _ = return $ Left $ NetworkError "fake outage"

--------------------------------------------------------------------------------
goodMovieFieldsTest = TestCase $ do
  result <- runTheMovieDBWithRequestFunction loadGoodMovieFile (fetch 0)
  case result of
    Left    err -> assertFailure $ show err
    Right movie -> do
      assertEqual "movieID" 105 (movieID movie)
      assertEqual "movieTitle" "Back to the Future" (movieTitle movie)
      assertBool "movieOverview" $
        "Eighties teenager" `isPrefixOf` movieOverview movie
      assertEqual "movieGenres" 4 (length $ movieGenres movie)
      assertEqual "moviePopularity" 80329.688 $ moviePopularity movie
      assertEqual "moviePosterPath" "/pTpxQB1N0waaSc3OSn0e9oc8kx9.jpg" $
        moviePosterPath movie
      assertEqual "movieReleaseDate" (Just $ fromGregorian 1985 7 3) $
        movieReleaseDate movie
      assertBool "movieAdult" $ not (movieAdult movie)
      assertEqual "movieIMDB" "tt0088763" $ movieIMDB movie
      assertEqual "movieRunTime" 116 $ movieRunTime movie

--------------------------------------------------------------------------------
badMovieJSONTest = TestCase $ do
  result <- runTheMovieDBWithRequestFunction loadBadMovieFile (fetch 0)
  case result of
    Left (ParseError e _) -> return ()
    _                     -> assertFailure "JSON should have been bad"

--------------------------------------------------------------------------------
shouldHaveNetworkErrorTest = TestCase $ do
  result <- runTheMovieDBWithRequestFunction fakeNetworkError (fetch 0)
  case result of
    Left (NetworkError e) -> return ()
    _                     -> assertFailure "should have network error"

--------------------------------------------------------------------------------
-- Why can't this be automatic?
unitTests = TestList
  [ TestLabel "All movie fields are parsed" goodMovieFieldsTest
  , TestLabel "Failure with bad JSON" badMovieJSONTest
  , TestLabel "Propagate network failure" shouldHaveNetworkErrorTest
  ]

--------------------------------------------------------------------------------
main = do counts <- runTestTT unitTests
          let bad = errors counts + failures counts
          when (bad > 0) exitFailure
