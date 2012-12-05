{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Types
       ( APIKey
       , APIError(..)
       , ReleaseDate(..)
       , GenreID
       , Genre(..)
       , MovieID
       , Movie(..)
       ) where

-- Imports.
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Applicative
import System.Locale
import Data.Time
import Data.Text (unpack)

-- | Type for the API Key issued by TheMovieDB.
type APIKey = String

-- | Possible errors returned by the API.
data APIError
  = NetworkError String -- ^ Network or HTTP error.
  | ParseError String   -- ^ Invalid or error response from the API.
  deriving (Eq, Show)

-- | Type wrapper for Day to parse a movie's release date.
newtype ReleaseDate =
  ReleaseDate {releaseDate :: Day}
  deriving (Eq, Show)

instance FromJSON ReleaseDate where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%Y-%m-%d" (unpack t) of
      Just d -> pure $ ReleaseDate d
      _      -> fail "could not parse release_date"
  parseJSON v = typeMismatch "ReleaseDate" v

-- | Type for representing unique genre IDs.
type GenreID = Int

-- | Metadata for a genre.
data Genre =
  Genre
  { genreID   :: GenreID -- ^ TheMovieDB unique ID.
  , genreName :: String  -- ^ The name of the genre.
  } deriving (Eq, Show)

instance FromJSON Genre where
  parseJSON (Object v) = Genre <$> v .: "id" <*> v .: "name"
  parseJSON v          = typeMismatch "Genre" v

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
