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
module Network.API.TheMovieDB.Actions
  ( searchMovies,
    fetchMovie,
    searchTV,
    fetchTV,
    fetchTVSeason,
    fetchFullTVSeries,
    config,
  )
where

import Network.API.TheMovieDB.Internal.Configuration
import Network.API.TheMovieDB.Internal.SearchResults
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Internal.Types
import Network.API.TheMovieDB.Types.Movie
import Network.API.TheMovieDB.Types.Season
import Network.API.TheMovieDB.Types.TV

-- | Search TheMovieDB using the given query string.
--
-- The movies returned will not have all their fields completely
-- filled out, to get a complete record you'll need to follow this
-- call up with a call to 'fetchMovie'.
searchMovies :: Text -> TheMovieDB [Movie]
searchMovies query = searchResults <$> search
  where
    search = getAndParse "search/movie" [("query", Just query)]

-- | Fetch the metadata for the 'Movie' with the given ID.
fetchMovie ::
  -- | TheMovieDB ID for the movie.
  ItemID ->
  TheMovieDB Movie
fetchMovie mid = getAndParse ("movie/" ++ show mid) []

-- | Search TheMovieDB for matching 'TV' series.
--
-- The 'TV' values returned from this function will be partial
-- records.  The only fields that will be available are 'tvID',
-- 'tvName', 'tvPosterPath', 'tvPopularity', and possibly
-- 'tvFirstAirDate'.
--
-- To get full 'TV' records you need to follow this function with a
-- call to 'fetchTV' using the desired 'tvID' value.
searchTV :: Text -> TheMovieDB [TV]
searchTV query = searchResults <$> search
  where
    search = getAndParse "search/tv" [("query", Just query)]

-- | Fetch metadata for a 'TV' series given its TheMovieDB ID.  The
-- metadata for 'Season's listed in the TV series will not have
-- complete 'Episode' information.
--
-- After calling this function you should call 'fetchTVSeason' to fill
-- in the 'Episode' metadata, or just begin with 'fetchFullTVSeries'.
fetchTV ::
  -- | TheMovieDB ID for the TV series.
  ItemID ->
  TheMovieDB TV
fetchTV i = getAndParse ("tv/" ++ show i) []

-- | Fetch metadata for a 'Season', including all 'Episode's.
fetchTVSeason ::
  -- | TheMovieDB ID for the TV series.
  ItemID ->
  -- | Season number (not season ID).
  Int ->
  TheMovieDB Season
fetchTVSeason i n = getAndParse ("tv/" ++ show i ++ "/season/" ++ show n) []

-- | Fetch full metadata for a 'TV' series, including all seasons and
-- episodes.
--
-- This function will make multiple HTTP requests to TheMovieDB API.
fetchFullTVSeries ::
  -- | TheMovieDB ID for the TV series.
  ItemID ->
  TheMovieDB TV
fetchFullTVSeries i = do
  tv <- fetchTV i
  seasons <- mapM (fetchTVSeason i . seasonNumber) (tvSeasons tv)
  return tv {tvSeasons = seasons}

-- | Fetch the API configuration information such as base URLs for
-- movie posters.  The resulting configuration value should be cached
-- and only requested every few days.
config :: TheMovieDB Configuration
config = getAndParse "configuration" []
