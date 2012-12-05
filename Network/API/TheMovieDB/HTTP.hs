{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.TheMovieDB.API.HTTP
       ( APIPath
       , APIQuery
       , BodyContent
       , apiGET
       ) where

import Network.API.TheMovieDB.Types
import Network.HTTP
import Network.URI
import qualified Data.ByteString.Lazy as B

type APIPath = String
type APIQuery = [(String, String)]
type BodyContent = B.ByteString

-- The base URL for the version of the API we're using.
apiBaseURL :: String
apiBaseURL = "http://api.themoviedb.org/3/"

-- Build a Network.HTTP.Request that can be used to access the API.
mkAPIRequest :: APIKey -> APIPath -> APIQuery -> Request BodyContent
mkAPIRequest key path params =
  case parseURI url of
    Nothing  -> error ("generated an invalid URI: " ++ url)
    Just uri -> insertHeaders headers $ request uri
  where query   = urlEncodeVars $ params ++ [("api_key", key)]
        url     = apiBaseURL ++ path ++ "?" ++ query
        headers = [Header HdrAccept "application/json"]
        request = mkRequest GET

-- | Build a URL and do an HTTP GET to TheMovieDB.
apiGET :: APIKey -> APIPath -> APIQuery -> IO (Either APIError BodyContent)
apiGET key path params =
  do result <- simpleHTTP $ mkAPIRequest key path params
     case result of
       Left err -> return $ Left  $ NetworkError (show err)
       Right r  -> return $ Right $ rspBody r
