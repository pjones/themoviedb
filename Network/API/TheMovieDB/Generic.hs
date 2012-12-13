{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Generic (getAndParse, getOrFail) where
import Network.API.TheMovieDB.Types.Context
import Network.API.TheMovieDB.Types.API
import Data.Aeson

-- Helper function to fetch and decode JSON.
getAndParse :: FromJSON a => Context -> Path -> Params -> IO (Either Error a)
getAndParse ctx path params =
  do httpResult <- (ioFunc ctx) (apiKey ctx) path params
     return $ case httpResult of
       Left  err  -> Left err
       Right body -> maybe (parseErr body) Right $ decode body
  where parseErr j = Left $ ParseError ("failed to parse JSON: " ++ show j)

-- Helper function to fail or return the right part of an either.
getOrFail :: FromJSON a => IO (Either Error a) -> IO a
getOrFail toCheck = toCheck >>= either (fail . show) return
