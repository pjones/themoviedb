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
--
-- Utility type for processing movie search results.
module Network.API.TheMovieDB.Internal.SearchResults
  ( SearchResults (..),
  )
where

import Data.Aeson

-- | Internal wrapper to parse a list of results from JSON.
newtype SearchResults a = SearchResults {searchResults :: [a]}
  deriving (Eq, Show)

instance (FromJSON a) => FromJSON (SearchResults a) where
  parseJSON = withObject "Search Results" $ \v ->
    SearchResults
      <$> v .: "results"
