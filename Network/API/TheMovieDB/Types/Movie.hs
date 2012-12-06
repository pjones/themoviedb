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
       ( MovieID
       , Movie(..)
       ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Time
import Network.API.TheMovieDB.Types.Genre
import Network.API.TheMovieDB.Types.ReleaseDate

-- | Type for representing unique movie IDs.
type MovieID = Int

-- | Metadata for a movie.
data Movie =
  Movie
  { movieID          :: MovieID     -- ^ TheMovieDB unique ID.
  , movieTitle       :: String      -- ^ The name/title of the movie.
  , movieOverview    :: String      -- ^ Short plot summary.
  , movieGenres      :: [Genre]     -- ^ List of genre names.
  , moviePopularity  :: Double      -- ^ Popularity ranking.
  , moviePosterPath  :: String      -- ^ FIXME:
  , movieReleaseDate :: ReleaseDate -- ^ Movie release date.
  } deriving (Eq, Show)

instance FromJSON Movie where
  parseJSON (Object v) = do
    genres <- maybe (pure []) parseJSON <$> (v .:? "genres")
    poster <- maybe (pure defaultPoster) posterURL <$> (v .:? "poster_path")
    Movie <$> v .:  "id"
          <*> v .:  "title"
          <*> v .:? "overview"     .!= ""
          <*> genres
          <*> v .:? "popularity"   .!= 0.0
          <*> poster
          <*> v .:? "release_date" .!= defaultDate
    where defaultDate   = ReleaseDate $ ModifiedJulianDay 0
          defaultPoster = "" :: String
          posterURL p   = parseJSON p
  parseJSON _ = empty
