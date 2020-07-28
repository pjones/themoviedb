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
module Network.API.TheMovieDB.Types.TV
  ( TV (..),
    tvPosterURLs,
  )
where

import Data.Aeson
import Data.Time (Day (..))
import Network.API.TheMovieDB.Internal.Configuration
import Network.API.TheMovieDB.Internal.Date
import Network.API.TheMovieDB.Internal.Types
import Network.API.TheMovieDB.Types.Genre
import Network.API.TheMovieDB.Types.Season

-- | Metadata for a TV series.
--
--   * The 'tvPosterPath' field is an incomplete URL.  To construct a
--     complete URL you'll need to use the 'Configuration' type and the
--    'tvPosterURLs' helper function.
data TV = TV
  { -- | TheMovieDB unique ID.
    tvID :: ItemID,
    -- | The name of the TV series.
    tvName :: Text,
    -- | Short description of the TV series.
    tvOverview :: Text,
    -- | List of 'Genre's.
    tvGenres :: [Genre],
    -- | Popularity ranking.
    tvPopularity :: Double,
    -- | Incomplete URL for poster image.  See 'tvPosterURLs'.
    tvPosterPath :: Text,
    -- | Air date for first episode.
    tvFirstAirDate :: Maybe Day,
    -- | Air date for last episode.
    tvLastAirDate :: Maybe Day,
    -- | Number of seasons for the TV series.
    tvNumberOfSeasons :: Int,
    -- | Total number of episodes for all seasons.
    tvNumberOfEpisodes :: Int,
    -- | Information about each season.
    --
    -- The number of elements in this list may not match
    -- 'tvNumberOfSeasons'.  Information about special episodes and
    -- unreleased episodes are usually kept in a 'Season' listed as
    -- season 0.  Therefore, the first element in this list might not
    -- be season 1.
    tvSeasons :: [Season]
  }
  deriving (Eq, Show)

instance Ord TV where
  compare a b = tvID a `compare` tvID b

instance FromJSON TV where
  parseJSON = withObject "TV" $ \v ->
    TV
      <$> v .: "id"
      <*> v .: "name"
      <*> v .:? "overview" .!= ""
      <*> v .:? "genres" .!= []
      <*> v .:? "popularity" .!= 0.0
      <*> v .:? "poster_path" .!= ""
      <*> v .:: "first_air_date"
      <*> v .:: "last_air_date"
      <*> v .:? "number_of_seasons" .!= 0
      <*> v .:? "number_of_episodes" .!= 0
      <*> v .:? "seasons" .!= []

-- | Return a list of URLs for all possible TV posters.
tvPosterURLs :: Configuration -> TV -> [Text]
tvPosterURLs c m = posterURLs c (tvPosterPath m)
