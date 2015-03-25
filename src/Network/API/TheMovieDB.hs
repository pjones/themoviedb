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

This library also includes an example executable in the @example@
directory.
-}
module Network.API.TheMovieDB
  ( -- * Types
    Movie(..)
  , Genre(..)
  , Error(..)
  , Key
  , MovieID
  , GenreID

    -- * API Functions
  , TheMovieDB
  , runTheMovieDB
  , runTheMovieDBWithManager
  , fetch
  , search

    -- * Utility Types and Functions
  , Configuration
  , config
  , moviePosterURLs
  ) where

import Network.API.TheMovieDB.Types
import Network.API.TheMovieDB.Config
import Network.API.TheMovieDB.Action
