{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

{- |

This library provides some data types and functions for fetching movie
metadata from <http://TheMovieDB.org>.  To use this library start by
requesting an API key from <http://docs.themoviedb.apiary.io>.

A typical workflow while using this library is:

  1. Place an API 'Key' inside a 'Context' using 'mkContext' or one of
     the utility functions in "Network.API.TheMovieDB.Util".

  2. Retrieve API 'Configuration' information using either the
     'config' function or the 'configErr' function.

  3. Search TheMovieDB using either the 'search' function or the
     'searchErr' function.

  4. Since the search functions don't return a full 'Movie' record
     follow them up with the 'fetch' function or the 'fetchErr'
     function.

This library also includes an example executable in the @example@
directory.
-}
module Network.API.TheMovieDB (
  -- * Types
  -- ** TheMovieDB Metadata
  Configuration(),
  Movie(..),
  Genre(..),

  -- ** Errors
  Error(..),

  -- ** Type Synonyms
  Key,
  MovieID,
  GenreID,

  -- * API Functions
  -- ** Functions That Fail
  config,
  fetch,
  search,

  -- ** Functions That Return Errors
  configErr,
  fetchErr,
  searchErr,

  -- ** Utility functions
  moviePosterURLs,
  runTheMovieDB,
  runTheMovieDBWithManager
  ) where

import Network.API.TheMovieDB.Types
import Network.API.TheMovieDB.Config
import Network.API.TheMovieDB.Fetch
import Network.API.TheMovieDB.Search
