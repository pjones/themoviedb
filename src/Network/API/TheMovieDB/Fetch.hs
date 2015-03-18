{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Fetch (fetchErr, fetch) where
import Network.API.TheMovieDB.Generic
import Network.API.TheMovieDB.Types

-- | Fetch the metadata for the movie with the given ID.  Returns
-- either an 'Error' or a 'Movie'.
fetchErr :: Context -> MovieID -> IO (Either Error Movie)
fetchErr ctx mid = getAndParse ctx ("movie/" ++ show mid) []

-- | Fetch the metadata for the movie with the given ID and fail if
-- any errors are encountered along the way.  For a function that
-- returns an error instead of failing see 'fetchErr'.
fetch :: Context -> MovieID -> IO Movie
fetch ctx mid = getOrFail $ fetchErr ctx mid
