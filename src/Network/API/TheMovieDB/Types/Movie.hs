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
module Network.API.TheMovieDB.Types.Movie
       ( Movie(..)
       , moviePosterURLs
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

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Metadata for a movie.
--
--   * The 'moviePosterPath' field is an incomplete URL.  To construct
--     a complete URL you'll need to use the 'Configuration' type and
--     the 'moviePosterURLs' helper function.
data Movie = Movie
  { movieID :: ItemID
    -- ^ TheMovieDB unique ID.

  , movieTitle :: Text
    -- ^ The name/title of the movie.

  , movieOverview :: Text
    -- ^ Short plot summary.

  , movieGenres :: [Genre]
    -- ^ List of 'Genre's.

  , moviePopularity :: Double
    -- ^ Popularity ranking.

  , moviePosterPath :: Text
    -- ^ Incomplete URL for poster image.  See 'moviePosterURLs'.

  , movieReleaseDate :: Maybe Day
    -- ^ Movie release date.  (Movie may not have been released yet.)

  , movieAdult :: Bool
    -- ^ TheMovieDB adult movie flag.

  , movieIMDB :: Text
    -- ^ IMDB.com ID.

  , movieRunTime :: Int
    -- ^ Movie length in minutes.

  } deriving (Eq, Show)

--------------------------------------------------------------------------------
instance FromJSON Movie where
  parseJSON (Object v) =
    Movie <$> v .:  "id"
          <*> v .:  "title"
          <*> v .:? "overview"    .!= ""
          <*> v .:? "genres"      .!= []
          <*> v .:? "popularity"  .!= 0.0
          <*> v .:? "poster_path" .!= ""
          <*> v .:: "release_date"
          <*> v .:? "adult"       .!= False
          <*> v .:? "imdb_id"     .!= ""
          <*> v .:? "runtime"     .!= 0
  parseJSON v = typeMismatch "Movie" v

--------------------------------------------------------------------------------
-- | Return a list of URLs for all possible movie posters.
moviePosterURLs :: Configuration -> Movie -> [Text]
moviePosterURLs c m = posterURLs c (moviePosterPath m)
