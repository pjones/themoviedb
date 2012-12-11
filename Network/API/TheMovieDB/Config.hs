{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Config
       ( configErr
       , config
       ) where

import Network.API.TheMovieDB.Types
import Network.API.TheMovieDB.HTTP
import Data.Aeson

-- | Fetch the API configuration information such as base URLs for
-- movie posters.  Results in either an error or a 'Configuration'.
configErr :: Context -> IO (Either Error Configuration)
configErr c = do response <- (ioFunc c) (apiKey c) "configuration" []
                 return $ case response of
                   Left err   -> Left err
                   Right body -> maybe (Left parseError) Right $ decode body
  where parseError = ParseError "failed to parse configuration JSON"


-- | Fetch the API configuration information or fail.
config :: Context -> IO Configuration
config c = do cfg <- configErr c
              either (fail . show) return cfg
