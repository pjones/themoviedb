{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Types.Context (Context(..), mkContext) where
import Network.API.TheMovieDB.Types.API
import Network.API.TheMovieDB.HTTP

-- | Data that needs to be given to the API functions.  Use the
-- 'mkContext' function to turn an API 'Key' into a 'Context'.
data Context = Context
  { apiKey :: Key    -- ^ Extract an API 'Key' from a 'Context'.
  , ioFunc :: IOFunc -- ^ The internal function that does the API IO.
  }

-- | Turns an API 'Key' into a 'Context' so that you can call the API
-- functions such as 'fetch' and 'search'.
mkContext :: Key -> Context
mkContext key = Context key apiGET
