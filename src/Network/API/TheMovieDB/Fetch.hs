{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.API.TheMovieDB.Fetch
       ( fetch
       ) where

--------------------------------------------------------------------------------
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Types

--------------------------------------------------------------------------------
-- | Fetch the metadata for the movie with the given ID.
fetch :: MovieID -> TheMovieDB Movie
fetch mid = getAndParse ("movie/" ++ show mid) []
