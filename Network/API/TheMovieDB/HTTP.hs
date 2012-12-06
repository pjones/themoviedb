{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.HTTP
       ( apiGET
       ) where

import Network.API.TheMovieDB.Types.API
import qualified Network.HTTP as H
import Network.URI

-- The base URL for the version of the API we're using.
apiBaseURL :: String
apiBaseURL = "http://api.themoviedb.org/3/"

-- Build a Network.HTTP.Request that can be used to access the API.
mkAPIRequest :: Key -> Path -> Params -> H.Request Body
mkAPIRequest key path params =
  case parseURI url of
    Nothing  -> error ("generated an invalid URI: " ++ url)
    Just uri -> H.insertHeaders headers $ request uri
  where query   = H.urlEncodeVars $ params ++ [("api_key", key)]
        url     = apiBaseURL ++ path ++ "?" ++ query
        headers = [H.Header H.HdrAccept "application/json"]
        request = H.mkRequest H.GET

-- | Build a URL and do an HTTP GET to TheMovieDB.
apiGET :: Key -> Path -> Params -> IO Response
apiGET key path params =
  do result <- H.simpleHTTP $ mkAPIRequest key path params
     return $ case result of
       Left err -> Left  $ NetworkError (show err)
       Right r  -> Right $ H.rspBody r
