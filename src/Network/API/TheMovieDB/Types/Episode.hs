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
module Network.API.TheMovieDB.Types.Episode
  ( Episode (..),
    episodeStillURLs,
  )
where

import Data.Aeson
import Data.Time (Day (..))
import Network.API.TheMovieDB.Internal.Configuration
import Network.API.TheMovieDB.Internal.Date
import Network.API.TheMovieDB.Internal.Types

-- | Metadata for a TV Episode.
--
--   * The 'episodeStillPath' field is an incomplete URL.  To
--     construct a complete URL you'll need to use the 'Configuration'
--     type and the 'episodeStillURLs' helper function.
data Episode = Episode
  { -- | TheMovieDB unique ID.
    episodeID :: ItemID,
    -- | Episode sequence number.
    episodeNumber :: Int,
    -- | Episode name.
    episodeName :: Text,
    -- | Short description of the episode.
    episodeOverview :: Text,
    -- | The season this episode belongs to.
    episodeSeasonNumber :: Int,
    -- | Episode air date, if it ever aired.
    episodeAirDate :: Maybe Day,
    -- | Incomplete URL to a still image from the episode.  See the
    -- 'episodeStillURLs' function for more information.
    episodeStillPath :: Text
  }
  deriving (Eq, Show)

instance Ord Episode where
  compare a b =
    compare
      (episodeSeasonNumber a, episodeNumber a)
      (episodeSeasonNumber b, episodeNumber b)

instance FromJSON Episode where
  parseJSON = withObject "Episode" $ \v ->
    Episode <$> v .: "id"
      <*> v .: "episode_number"
      <*> v .: "name"
      <*> v .: "overview"
      <*> v .: "season_number" .!= 0
      <*> v .:: "air_date"
      <*> v .: "still_path" .!= ""

-- | Return a list of URLs for all possible episode still images.
episodeStillURLs :: Configuration -> Episode -> [Text]
episodeStillURLs c e = posterURLs c (episodeStillPath e)
