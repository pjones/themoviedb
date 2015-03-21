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
module Network.API.TheMovieDB.Search
       ( searchErr
       , search
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import Network.API.TheMovieDB.Internal.Generic
import Network.API.TheMovieDB.Types

--------------------------------------------------------------------------------
-- Internal wrapper to parse a list of movies from JSON.
newtype SearchResults = SearchResults {searchResults :: [Movie]}
                        deriving (Eq, Show)

--------------------------------------------------------------------------------
instance FromJSON SearchResults where
  parseJSON (Object v) = SearchResults <$> v .: "results"
  parseJSON _          = empty

--------------------------------------------------------------------------------
-- | Internal function to translate search results to a list of movies.
fetchSearchResults :: Text -> TheMovieDB (Either Error SearchResults)
fetchSearchResults query = getAndParse "search/movie" [("query", Just query)]

--------------------------------------------------------------------------------
-- | Search TheMovieDB using the given query string and return either
-- an 'Error' if something went wrong or a list of matching 'Movie's.
--
-- The movies returned will not have all their fields completely
-- filled out, to get a complete record you'll need to follow this
-- call up with a call to 'fetchErr' or 'fetch'.
searchErr :: Text -> TheMovieDB (Either Error [Movie])
searchErr query = (fmap searchResults) <$> fetchSearchResults query

--------------------------------------------------------------------------------
-- | Search TheMovieDB using the given query string and return a list
-- of movies.  This function fails if there are any errors.  For a
-- function that returns an error instead of failing see 'searchErr'.
--
-- The movies returned will not have all their fields completely
-- filled out, to get a complete record you'll need to follow this
-- call up with a call to 'fetchErr' or 'fetch'.
search :: Text -> TheMovieDB [Movie]
search query = getOrFail (searchErr query)
