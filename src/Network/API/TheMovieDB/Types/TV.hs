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
module Network.API.TheMovieDB.Types.TV
       ( TV(..)
       , tvPosterURLs
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
import Network.API.TheMovieDB.Types.Genre
import Network.API.TheMovieDB.Types.Season

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Metadata for a TV series.
--
--   * The 'tvPosterPath' field is an incomplete URL.  To construct a
--     complete URL you'll need to use the 'Configuration' type and the
--    'tvPosterURLs' helper function.
data TV = TV
  { tvID :: ItemID
    -- ^ TheMovieDB unique ID.

  , tvName :: Text
    -- ^ The name of the TV series.

  , tvOverview :: Text
    -- ^ Short description of the TV series.

  , tvGenres :: [Genre]
    -- ^ List of 'Genre's.

  , tvPopularity :: Double
    -- ^ Popularity ranking.

  , tvPosterPath :: Text
    -- ^ Incomplete URL for poster image.  See 'tvPosterURLs'.

  , tvFirstAirDate :: Maybe Day
    -- ^ Air date for first episode.

  , tvLastAirDate :: Maybe Day
    -- ^ Air date for last episode.

  , tvNumberOfSeasons :: Int
    -- ^ Number of seasons for the TV series.

  , tvNumberOfEpisodes :: Int
    -- ^ Total number of episodes for all seasons.

  , tvSeasons :: [Season]
    -- ^ Information about each season.
    --
    -- The number of elements in this list may not match
    -- 'tvNumberOfSeasons'.  Information about special episodes and
    -- unreleased episodes are usually kept in a 'Season' listed as
    -- season 0.  Therefore, the first element in this list might not
    -- be season 1.

  } deriving (Eq, Show)

--------------------------------------------------------------------------------
instance Ord TV where
  compare a b = tvID a `compare` tvID b

--------------------------------------------------------------------------------
instance FromJSON TV where
  parseJSON (Object v) =
    TV <$> v .:  "id"
       <*> v .:  "name"
       <*> v .:? "overview"           .!= ""
       <*> v .:? "genres"             .!= []
       <*> v .:? "popularity"         .!= 0.0
       <*> v .:? "poster_path"        .!= ""
       <*> v .:: "first_air_date"
       <*> v .:: "last_air_date"
       <*> v .:? "number_of_seasons"  .!= 0
       <*> v .:? "number_of_episodes" .!= 0
       <*> v .:? "seasons"            .!= []
  parseJSON v = typeMismatch "TV" v

--------------------------------------------------------------------------------
-- | Return a list of URLs for all possible TV posters.
tvPosterURLs :: Configuration -> TV -> [Text]
tvPosterURLs c m = posterURLs c (tvPosterPath m)
