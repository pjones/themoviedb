{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.API.TheMovieDB.Types.Episode
       ( Episode (..)
       , episodeStillURLs
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Data.Time (Day (..))
import Network.API.TheMovieDB.Internal.Configuration
import Network.API.TheMovieDB.Internal.Date
import Network.API.TheMovieDB.Internal.Types

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Metadata for a TV Episode.
--
--   * The 'episodeStillPath' field is an incomplete URL.  To
--     construct a complete URL you'll need to use the 'Configuration'
--     type and the 'episodeStillURLs' helper function.
data Episode = Episode
  { episodeID :: ItemID
    -- ^ TheMovieDB unique ID.

  , episodeNumber :: Int
    -- ^ Episode sequence number.

  , episodeName :: Text
    -- ^ Episode name.

  , episodeOverview :: Text
    -- ^ Short description of the episode.

  , episodeSeasonNumber :: Int
    -- ^ The season this episode belongs to.

  , episodeAirDate :: Maybe Day
    -- ^ Episode air date, if it ever aired.

  , episodeStillPath :: Text
    -- ^ Incomplete URL to a still image from the episode.  See the
    -- 'episodeStillURLs' function for more information.

  } deriving (Eq, Show)

--------------------------------------------------------------------------------
instance Ord Episode where
  compare a b = compare (episodeSeasonNumber a, episodeNumber a)
                        (episodeSeasonNumber b, episodeNumber b)

--------------------------------------------------------------------------------
instance FromJSON Episode where
  parseJSON (Object v) =
    Episode <$> v .:  "id"
            <*> v .: "episode_number"
            <*> v .: "name"
            <*> v .: "overview"
            <*> v .:  "season_number" .!= 0
            <*> v .:: "air_date"
            <*> v .:  "still_path"    .!= ""
  parseJSON v = typeMismatch "Episode" v

--------------------------------------------------------------------------------
-- | Return a list of URLs for all possible episode still images.
episodeStillURLs :: Configuration -> Episode -> [Text]
episodeStillURLs c e = posterURLs c (episodeStillPath e)
