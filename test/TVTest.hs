{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module TVTest (tests) where

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import TestHelper

--------------------------------------------------------------------------------
import qualified Data.Text as T
import Network.API.TheMovieDB

--------------------------------------------------------------------------------
testTVBasicFields :: TV -> Assertion
testTVBasicFields tv = do
  assertEqual "id"     1437                               (tvID   tv)
  assertEqual "name"   "Firefly"                          (tvName tv)
  assertEqual "poster" "/mWNadwBZIx8NyEw4smGftYtHHrE.jpg" (tvPosterPath tv)
  assertEqual "aired"  (mkDay 2002 12 20)                 (tvFirstAirDate tv)

--------------------------------------------------------------------------------
testSearchTV :: Assertion
testSearchTV = do
  tvs <- fakeTMDB "test/search-tv-good.json" (searchTV T.empty)
  assertEqual "length" 1 (length tvs)
  mapM_ testTVBasicFields tvs

--------------------------------------------------------------------------------
testFetchTV :: Assertion
testFetchTV = do
  tv <- fakeTMDB "test/tv-good.json" (fetchTV 0)
  testTVBasicFields tv
  assertEqual "genres"     2  (length $ tvGenres tv)
  assertEqual "seasons"    1  (tvNumberOfSeasons tv)
  assertEqual "episodes"   13 (tvNumberOfEpisodes tv)
  assertEqual "season len" 2  (length $ tvSeasons tv)

--------------------------------------------------------------------------------
testFetchSeason :: Assertion
testFetchSeason = do
  season <- fakeTMDB "test/season-good.json" (fetchTVSeason 0 0)
  assertEqual "length" 14 (length $ seasonEpisodes season)

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "TV"
  [ testCase "Search fields" testSearchTV
  , testCase "Fetch fields"  testFetchTV
  , testCase "Season fields" testFetchSeason
  ]
