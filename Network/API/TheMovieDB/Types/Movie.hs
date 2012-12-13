{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Types.Movie
       (MovieID, Movie(..), moviePosterURLs) where

import Control.Applicative
import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Time (Day(..))
import Network.API.TheMovieDB.Types.Configuration (Configuration(..))
import Network.API.TheMovieDB.Types.Genre (Genre(..))
import Network.API.TheMovieDB.Types.ReleaseDate (ReleaseDate(..))

-- | Type for representing unique movie IDs.
type MovieID = Int

-- | Metadata for a movie.
--
--   * The 'moviePosterPath' is an incomplete URL.  To construct a
--     complete URL you'll need to use the 'Configuration' type.  You
--     can also use 'moviePosterURLs'.
data Movie = Movie
  { movieID          :: MovieID -- ^ TheMovieDB unique ID.
  , movieTitle       :: String  -- ^ The name/title of the movie.
  , movieOverview    :: String  -- ^ Short plot summary.
  , movieGenres      :: [Genre] -- ^ List of genre names.
  , moviePopularity  :: Double  -- ^ Popularity ranking.
  , moviePosterPath  :: String  -- ^ Incomplete URL for poster image.
  , movieReleaseDate :: Day     -- ^ Movie release date.
  , movieAdult       :: Bool    -- ^ TheMovieDB adult flag.
  , movieIMDB        :: String  -- ^ IMDB.com ID
  , movieRunTime     :: Int     -- ^ Movie length in minutes.
  } deriving (Eq, Show)

instance FromJSON Movie where
  parseJSON (Object v) =
    Movie <$> v .:  "id"
          <*> v .:  "title"
          <*> v .:? "overview"    .!= ""
          <*> v .:? "genres"      .!= []
          <*> v .:? "popularity"  .!= 0.0
          <*> v .:? "poster_path" .!= ""
          <*> liftM releaseDate (v .: "release_date")
          <*> v .:? "adult"       .!= False
          <*> v .:? "imdb_id"     .!= ""
          <*> v .:? "runtime"     .!= 0
  parseJSON _ = empty

-- | Return a list of URLs for all possible movie posters.
moviePosterURLs :: Configuration -> Movie -> [String]
moviePosterURLs c m = [base ++ size ++ poster | size <- cfgPosterSizes c]
  where base   = cfgImageBaseURL c
        poster = moviePosterPath m
