-- |
--
-- Copyright:
--   This file is part of the package themoviedb.  It is subject to
--   the license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/themoviedb
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: MIT
module MovieTest (tests) where

import qualified Data.Text as Text
import Network.API.TheMovieDB
import Network.API.TheMovieDB.Internal.TheMovieDB
import Test.Tasty
import Test.Tasty.HUnit
import TestHelper

testSearchMovie :: Assertion
testSearchMovie = do
  movies <- fakeTMDB "test/search-good.json" (searchMovies mempty)
  assertEqual "length" 8 (length movies)

testMoviePosterURLs :: Assertion
testMoviePosterURLs = do
  cfg <- fakeTMDB "test/config-good.json" config
  movie <- fakeTMDB "test/movie-good.json" (fetchMovie 0)
  let urls = moviePosterURLs cfg movie
      poster = "http://cf2.imgobject.com/t/p/w92/pTpxQB1N0waaSc3OSn0e9oc8kx9.jpg"
  assertEqual "length" 6 (length urls)
  assertEqual "url" (Just poster) (viaNonEmpty head urls)

goodMovieFieldsTest :: Assertion
goodMovieFieldsTest = do
  movie <- fakeTMDB "test/movie-good.json" (fetchMovie 0)
  assertEqual "movieID" 105 (movieID movie)
  assertEqual "movieTitle" "Back to the Future" (movieTitle movie)
  assertBool "movieOverview" $ "Eighties teenager" `Text.isPrefixOf` movieOverview movie
  assertEqual "movieGenres" 4 (length $ movieGenres movie)
  assertEqual "moviePopularity" 80329.688 $ moviePopularity movie
  assertEqual "moviePosterPath" "/pTpxQB1N0waaSc3OSn0e9oc8kx9.jpg" $ moviePosterPath movie
  assertEqual "movieReleaseDate" (mkDay 1985 7 3) (movieReleaseDate movie)
  assertBool "movieAdult" $ not (movieAdult movie)
  assertEqual "movieIMDB" "tt0088763" $ movieIMDB movie
  assertEqual "movieRunTime" 116 $ movieRunTime movie

badMovieJSONTest :: Assertion
badMovieJSONTest = do
  result <-
    runTheMovieDBWithRequestFunction
      (fileRequest "test/movie-bad.json")
      (fetchMovie 0)

  case result of
    Left (ResponseParseError _ _) -> return ()
    _ -> assertFailure "JSON should have been bad"

shouldHaveNetworkErrorTest :: Assertion
shouldHaveNetworkErrorTest = do
  result <- runTheMovieDBWithRequestFunction fakeNetworkError (fetchMovie 0)
  case result of
    Left (ServiceError _) -> return ()
    _ -> assertFailure "should have network error"

tests :: TestTree
tests =
  testGroup
    "Movies"
    [ testCase "All movie fields are parsed" goodMovieFieldsTest,
      testCase "Failure with bad JSON" badMovieJSONTest,
      testCase "Propagate network failure" shouldHaveNetworkErrorTest,
      testCase "Searching movies" testSearchMovie,
      testCase "Movie poster URLs" testMoviePosterURLs
    ]
