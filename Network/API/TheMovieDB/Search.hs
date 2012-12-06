{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Search
       ( SearchQuery
       , searchErr
       , search
       ) where

import Network.API.TheMovieDB.Types
import Network.API.TheMovieDB.Types.API
import Network.API.TheMovieDB.HTTP
import Data.Aeson
import Control.Applicative

-- | A search query for TheMovieDB API.
type SearchQuery = String

-- Internal wrapper to parse a list of movies from JSON.
newtype SearchResults =
  SearchResults {searchResults :: [Movie]}
  deriving (Eq, Show)

instance FromJSON SearchResults where
  parseJSON (Object v) =
    do movies <- (v .: "results") >>= parseJSON
       return $ SearchResults movies
  parseJSON _ = empty

-- Internal function to parse the body of a search.  I'd like to move
-- this into searchErr at some point.
moviesFromSearchJSON :: Body -> Either Error [Movie]
moviesFromSearchJSON body =
  case decode body of
    Nothing -> Left parseError
    Just sr -> Right (searchResults sr)
  where parseError = ParseError ("failed to parse search results" ++ show body)

-- | Search TheMovieDB using the given query string returning either
--   an error if something went wrong or a list of matching movies.
--   The movies returned will not have all their fields, to get a
--   complete record you'll need to follow this call up with a call to
--   'fetchErr' or 'fetch'.
searchErr :: Config -> SearchQuery -> IO (Either Error [Movie])
searchErr c q = do
  result <- (ioFunc c) (apiKey c) "search/movie" [("query", q)]
  return $ either (Left . NetworkError . show) moviesFromSearchJSON result

-- | Similar to 'searchErr' except the results are a list of movies
--   and in the case of an error the list will be empty.
search :: Config -> SearchQuery -> IO [Movie]
search c q = do result <- searchErr c q
                either (const $ return []) return result
