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

Example:

@
import Network.API.TheMovieDB

main :: IO ()
main = do
  -- The API key assigned to you (as a 'Text' value).
  let key = "your API key"

  -- The 'fetch' function will get a 'Movie' record based on its ID.
  result <- runTheMovieDB key (fetchMovie 9340)

  -- Do something with the result (or error).
  putStrLn (show result)
@

This library also includes an example executable in the @example@
directory.
-}
module Network.API.TheMovieDB
  ( -- * Types
    Movie (..)
  , TV (..)
  , Season (..)
  , Episode (..)
  , Genre (..)
  , Error (..)
  , ItemID
  , Key

    -- * API Functions
  , TheMovieDB
  , runTheMovieDB
  , runTheMovieDBWithManager
  , searchMovies
  , fetchMovie
  , searchTV
  , fetchTV
  , fetchTVSeason
  , fetchFullTVSeries

    -- * Utility Types and Functions
  , Configuration
  , config
  , moviePosterURLs
  , tvPosterURLs
  , seasonPosterURLs
  , episodeStillURLs
  ) where

--------------------------------------------------------------------------------
import Network.API.TheMovieDB.Actions
import Network.API.TheMovieDB.Internal.Configuration
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Internal.Types
import Network.API.TheMovieDB.Types.Episode
import Network.API.TheMovieDB.Types.Genre
import Network.API.TheMovieDB.Types.Movie
import Network.API.TheMovieDB.Types.Season
import Network.API.TheMovieDB.Types.TV
