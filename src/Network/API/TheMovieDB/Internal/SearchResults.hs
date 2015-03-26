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
-- | Utility type for processing movie search results.
module Network.API.TheMovieDB.Internal.SearchResults
       ( SearchResults (..)
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Network.API.TheMovieDB.Types

--------------------------------------------------------------------------------
-- | Internal wrapper to parse a list of movies from JSON.
newtype SearchResults = SearchResults {searchResults :: [Movie]}
                        deriving (Eq, Show)

--------------------------------------------------------------------------------
instance FromJSON SearchResults where
  parseJSON (Object v) = SearchResults <$> v .: "results"
  parseJSON _          = empty
