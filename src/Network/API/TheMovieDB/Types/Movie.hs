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
module Network.API.TheMovieDB.Types.Movie
  ( Movie (..),
    moviePosterURLs,
  )
where

import Data.Aeson
import Data.Time (Day (..))
import Network.API.TheMovieDB.Internal.Configuration
import Network.API.TheMovieDB.Internal.Date
import Network.API.TheMovieDB.Internal.Types
import Network.API.TheMovieDB.Types.Genre

-- | Metadata for a movie.
--
--   * The 'moviePosterPath' field is an incomplete URL.  To construct
--     a complete URL you'll need to use the 'Configuration' type and
--     the 'moviePosterURLs' helper function.
data Movie = Movie
  { -- | TheMovieDB unique ID.
    movieID :: ItemID,
    -- | The name/title of the movie.
    movieTitle :: Text,
    -- | Short plot summary.
    movieOverview :: Text,
    -- | List of 'Genre's.
    movieGenres :: [Genre],
    -- | Popularity ranking.
    moviePopularity :: Double,
    -- | Incomplete URL for poster image.  See 'moviePosterURLs'.
    moviePosterPath :: Text,
    -- | Movie release date.  (Movie may not have been released yet.)
    movieReleaseDate :: Maybe Day,
    -- | TheMovieDB adult movie flag.
    movieAdult :: Bool,
    -- | IMDB.com ID.
    movieIMDB :: Text,
    -- | Movie length in minutes.
    movieRunTime :: Int
  }
  deriving (Eq, Show)

instance FromJSON Movie where
  parseJSON = withObject "Movie" $ \v ->
    Movie
      <$> v .: "id"
      <*> v .: "title"
      <*> v .:? "overview" .!= ""
      <*> v .:? "genres" .!= []
      <*> v .:? "popularity" .!= 0.0
      <*> v .:? "poster_path" .!= ""
      <*> v .:: "release_date"
      <*> v .:? "adult" .!= False
      <*> v .:? "imdb_id" .!= ""
      <*> v .:? "runtime" .!= 0

-- | Return a list of URLs for all possible movie posters.
moviePosterURLs :: Configuration -> Movie -> [Text]
moviePosterURLs c m = posterURLs c (moviePosterPath m)
