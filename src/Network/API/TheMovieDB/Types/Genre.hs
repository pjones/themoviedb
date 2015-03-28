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
module Network.API.TheMovieDB.Types.Genre
       ( Genre(..)
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Network.API.TheMovieDB.Internal.Types

--------------------------------------------------------------------------------
-- | Metadata for a genre.
data Genre = Genre
  { genreID   :: ItemID -- ^ TheMovieDB unique ID.
  , genreName :: Text   -- ^ The name of the genre.
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
instance FromJSON Genre where
  parseJSON (Object v) = Genre <$> v .: "id" <*> v .: "name"
  parseJSON v          = typeMismatch "Genre" v
