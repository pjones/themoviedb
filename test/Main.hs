{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
-- http://www.haskell.org/haskellwiki/HUnit_1.0_User%27s_Guide
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Data.Time (fromGregorian)
import Network.API.TheMovieDB
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Internal.Types
import System.Exit (exitFailure)

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
fakeNetworkError _ _ = return $ Left $ ServiceError "fake outage"

--------------------------------------------------------------------------------
goodMovieFieldsTest = TestCase $ do
  result <- runTheMovieDBWithRequestFunction loadGoodMovieFile (fetchMovie 0)
  case result of
    Left    err -> assertFailure $ show err
    Right movie -> do
      assertEqual "movieID" 105 (movieID movie)
      assertEqual "movieTitle" "Back to the Future" (movieTitle movie)
      assertBool "movieOverview" $
        "Eighties teenager" `T.isPrefixOf` movieOverview movie
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
  result <- runTheMovieDBWithRequestFunction loadBadMovieFile (fetchMovie 0)
  case result of
    Left (ResponseParseError e _) -> return ()
    _                             -> assertFailure "JSON should have been bad"

--------------------------------------------------------------------------------
shouldHaveNetworkErrorTest = TestCase $ do
  result <- runTheMovieDBWithRequestFunction fakeNetworkError (fetchMovie 0)
  case result of
    Left (ServiceError e) -> return ()
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
