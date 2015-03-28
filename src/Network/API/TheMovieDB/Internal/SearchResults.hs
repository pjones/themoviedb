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

--------------------------------------------------------------------------------
-- | Internal wrapper to parse a list of results from JSON.
newtype SearchResults a = SearchResults {searchResults :: [a]}
                          deriving (Eq, Show)

--------------------------------------------------------------------------------
instance (FromJSON a) => FromJSON (SearchResults a) where
  parseJSON (Object v) = SearchResults <$> v .: "results"
  parseJSON _          = empty
