{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Types.Config
       ( Config(..)
       , mkConfig
       ) where

import Network.API.TheMovieDB.Types.API
import Network.API.TheMovieDB.HTTP

-- | Configuration data that needs to be given to the API functions.
--   Use the 'mkConfig' function to turn an API 'Key' into a 'Config'.
data Config =
  Config
  { apiKey :: Key
  , ioFunc :: IOFunc
  }

-- | Turns an API 'Key' into a 'Config' so that you can call the API
--   functions such as 'fetch' and 'search'.
mkConfig :: Key -> Config
mkConfig key = Config key apiGET
