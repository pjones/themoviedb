{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.API.TheMovieDB.Internal.Generic
       ( getAndParse
       , getOrFail
       ) where

--------------------------------------------------------------------------------
import Data.Aeson
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Internal.Types
import Network.HTTP.Types

--------------------------------------------------------------------------------
-- Helper function to fetch and decode JSON.
getAndParse :: FromJSON a => Path -> QueryText -> TheMovieDB (Either Error a)
getAndParse path params =
  do httpResult <- runRequest path params
     return (either (Left . id) doParse $ httpResult)
  where
    doParse b = either (Left . parseErr b) Right $ eitherDecode b
    parseErr b e = ParseError ("failed to parse JSON response: " ++ e) (Just b)

--------------------------------------------------------------------------------
-- Helper function to fail or return the right part of an either.
getOrFail :: FromJSON a => TheMovieDB (Either Error a) -> TheMovieDB a
getOrFail toCheck = toCheck >>= either (fail . show) return
