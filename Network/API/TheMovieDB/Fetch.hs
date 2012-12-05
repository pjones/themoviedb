{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Fetch
       ( fetchErr
       , fetch
       ) where

-- Imports.
import Network.API.TheMovieDB.Types
import Network.API.TheMovieDB.HTTP
import Data.Aeson

-- | Fetch the metadata for the movie with the given ID.  Returns
--   either an APIError or a Movie.
fetchErr :: APIKey -> MovieID -> IO (Either APIError Movie)
fetchErr key movieID =
  do r <- apiGET key ("movie/" ++ show movieID) []
     case r of
       Left err   -> return $ Left err
       Right body -> return $ maybe (Left parseError) Right $ decode body
  where parseError = ParseError "failed to parse movie JSON"

-- | Fetch the metadata for the movie with the given ID and fail if
--   any errors are encountered along the way.
fetch :: APIKey -> MovieID -> IO Movie
fetch key movieID =
  do r <- fetchErr key movieID
     either (fail . show) return r
