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
       ( search
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Text (Text)
import Network.API.TheMovieDB.Internal.SearchResults
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Types

--------------------------------------------------------------------------------
-- | Internal function to translate search results to a list of movies.
fetchSearchResults :: Text -> TheMovieDB SearchResults
fetchSearchResults query = getAndParse "search/movie" [("query", Just query)]

--------------------------------------------------------------------------------
-- | Search TheMovieDB using the given query string.
--
-- The movies returned will not have all their fields completely
-- filled out, to get a complete record you'll need to follow this
-- call up with a call to 'fetch'.
search :: Text -> TheMovieDB [Movie]
search query = searchResults <$> fetchSearchResults query
