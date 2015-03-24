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
       ) where

--------------------------------------------------------------------------------
import Data.Aeson
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Internal.Types
import Network.HTTP.Types

--------------------------------------------------------------------------------
-- Helper function to fetch and decode JSON.
getAndParse :: FromJSON a => Path -> QueryText -> TheMovieDB a
getAndParse path params = do
  body <- runRequest path params

  case eitherDecode body of
    Left  e -> apiError $ ParseError ("bad JSON: " ++ e) (Just body)
    Right a -> return a
