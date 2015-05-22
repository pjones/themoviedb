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
module Network.API.TheMovieDB.Types.Season
       ( Season (..)
       , seasonPosterURLs
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
import Network.API.TheMovieDB.Types.Episode

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Metadata for a TV Season.
--
--   * The 'seasonPosterPath' field is an incomplete URL.  To
--     construct a complete URL you'll need to use the 'Configuration'
--     type and the 'seasonPosterURLs' helper function.
data Season = Season
  { seasonID :: ItemID
    -- ^ TheMovieDB unique ID.

  , seasonNumber :: Int
    -- ^ Season sequence number.  Remember that season 0 is sometimes
    -- used to hold unreleased/unaired episodes.

  , seasonAirDate :: Maybe Day
    -- ^ The date this season began to air, if ever.

  , seasonEpisodeCount :: Int
    -- ^ Number of episodes in this season.

  , seasonPosterPath :: Text
    -- ^ Incomplete URL for poster image.  See 'seasonPosterURLs'.

  , seasonEpisodes :: [Episode]
    -- ^ List of 'Episode's.

  } deriving (Eq, Show)

--------------------------------------------------------------------------------
instance Ord Season where
  compare a b = seasonNumber a `compare` seasonNumber b

--------------------------------------------------------------------------------
instance FromJSON Season where
  parseJSON (Object v) =
    Season <$> v .:  "id"
           <*> v .:  "season_number" .!= 0
           <*> v .:: "air_date"
           <*> v .:? "episode_count" .!= 0
           <*> v .:  "poster_path"   .!= ""
           <*> v .:? "episodes"      .!= []
  parseJSON v = typeMismatch "Season" v

--------------------------------------------------------------------------------
-- | Return a list of URLs for all possible season posters.
seasonPosterURLs :: Configuration -> Season -> [Text]
seasonPosterURLs c s = posterURLs c (seasonPosterPath s)
