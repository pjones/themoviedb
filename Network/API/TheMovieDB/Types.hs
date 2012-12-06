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
import Network.API.TheMovieDB.Types.Genre
import Network.API.TheMovieDB.Types.Movie
import Network.API.TheMovieDB.Types.ReleaseDate

-- | Type for the API key issued by TheMovieDB.
type APIKey = String

-- | Possible errors returned by the API.
data APIError
  = NetworkError String -- ^ Network or HTTP error.
  | ParseError String   -- ^ Invalid or error response from the API.
  deriving (Eq, Show)

-- data Config =
--   Config
--   { cfgAPIKey :: Key
--   , cfgIOFunc :: Config -> Query -> IO (Either APIError BodyContent)
--   }
