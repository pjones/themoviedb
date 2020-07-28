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
module TVTest (tests) where

import Network.API.TheMovieDB
import Network.API.TheMovieDB.Internal.HTTP (defaultImagePrefix)
import Test.Tasty
import Test.Tasty.HUnit
import TestHelper

testTVBasicFields :: TV -> Assertion
testTVBasicFields tv = do
  assertEqual "id" 1437 (tvID tv)
  assertEqual "name" "Firefly" (tvName tv)
  assertEqual "poster" "/mWNadwBZIx8NyEw4smGftYtHHrE.jpg" (tvPosterPath tv)
  assertEqual "aired" (mkDay 2002 12 20) (tvFirstAirDate tv)

testSearchTV :: Assertion
testSearchTV = do
  tvs <- fakeTMDB "test/search-tv-good.json" (searchTV mempty)
  assertEqual "length" 1 (length tvs)
  mapM_ testTVBasicFields tvs

testFetchTV :: Assertion
testFetchTV = do
  tv <- fakeTMDB "test/tv-good.json" (fetchTV 0)
  testTVBasicFields tv
  assertEqual "genres" 2 (length $ tvGenres tv)
  assertEqual "seasons" 1 (tvNumberOfSeasons tv)
  assertEqual "episodes" 13 (tvNumberOfEpisodes tv)
  assertEqual "season len" 2 (length $ tvSeasons tv)

testFetchSeason :: Assertion
testFetchSeason = do
  season <- fakeTMDB "test/season-good.json" (fetchTVSeason 0 0)
  assertEqual "length" 14 (length $ seasonEpisodes season)

testSeasonPoster :: Assertion
testSeasonPoster = do
  cfg <- fakeTMDB "test/config-good.json" config
  season <- fakeTMDB "test/season-good.json" (fetchTVSeason 0 0)

  let expect = defaultImagePrefix <> "w92/2dxsbVMoxsYH0Pta2mbFjF7mhHr.jpg"
      urls = seasonPosterURLs cfg season

  assertEqual "poster" (Just expect) (viaNonEmpty head urls)

testTVPoster :: Assertion
testTVPoster = do
  cfg <- fakeTMDB "test/config-good.json" config
  tv <- fakeTMDB "test/tv-good.json" (fetchTV 0)

  let expect = defaultImagePrefix <> "w92/mWNadwBZIx8NyEw4smGftYtHHrE.jpg"
      urls = tvPosterURLs cfg tv

  assertEqual "poster" (Just expect) (viaNonEmpty head urls)

tests :: TestTree
tests =
  testGroup
    "TV"
    [ testCase "Search fields" testSearchTV,
      testCase "Fetch fields" testFetchTV,
      testCase "Season fields" testFetchSeason,
      testCase "Season Poster" testSeasonPoster,
      testCase "TV Poster" testTVPoster
    ]
