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
module Network.API.TheMovieDB.Types.Season
  ( Season (..),
    seasonPosterURLs,
  )
where

import Data.Aeson
import Data.Time (Day (..))
import Network.API.TheMovieDB.Internal.Configuration
import Network.API.TheMovieDB.Internal.Date
import Network.API.TheMovieDB.Internal.Types
import Network.API.TheMovieDB.Types.Episode

-- | Metadata for a TV Season.
--
--   * The 'seasonPosterPath' field is an incomplete URL.  To
--     construct a complete URL you'll need to use the 'Configuration'
--     type and the 'seasonPosterURLs' helper function.
data Season = Season
  { -- | TheMovieDB unique ID.
    seasonID :: ItemID,
    -- | Season sequence number.  Remember that season 0 is sometimes
    -- used to hold unreleased/unaired episodes.
    seasonNumber :: Int,
    -- | The date this season began to air, if ever.
    seasonAirDate :: Maybe Day,
    -- | Number of episodes in this season.
    seasonEpisodeCount :: Int,
    -- | Incomplete URL for poster image.  See 'seasonPosterURLs'.
    seasonPosterPath :: Text,
    -- | List of 'Episode's.
    seasonEpisodes :: [Episode]
  }
  deriving (Eq, Show)

instance Ord Season where
  compare a b = seasonNumber a `compare` seasonNumber b

instance FromJSON Season where
  parseJSON = withObject "Season" $ \v ->
    Season
      <$> v .: "id"
      <*> v .: "season_number" .!= 0
      <*> v .:: "air_date"
      <*> v .:? "episode_count" .!= 0
      <*> v .: "poster_path" .!= ""
      <*> v .:? "episodes" .!= []

-- | Return a list of URLs for all possible season posters.
seasonPosterURLs :: Configuration -> Season -> [Text]
seasonPosterURLs c s = posterURLs c (seasonPosterPath s)
