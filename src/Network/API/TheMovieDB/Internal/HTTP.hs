{-# LANGUAGE  OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Simple interface for fetching JSON files from the API via HTTP.
module Network.API.TheMovieDB.Internal.HTTP
       ( apiGET
       ) where

--------------------------------------------------------------------------------
import Network.API.TheMovieDB.Internal.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types

--------------------------------------------------------------------------------
-- | The base URL for the version of the API we're using.
apiBaseURL :: String
apiBaseURL = "https://api.themoviedb.org/3/"

--------------------------------------------------------------------------------
-- | Build a HTTP request that can be used to access the API.
mkAPIRequest :: Key -> Path -> QueryText -> IO HC.Request
mkAPIRequest key path params = do
  req <- HC.parseUrl (apiBaseURL ++ path)

  return $ req { HC.queryString    = query 
               , HC.requestHeaders = headers
               }
  
  where
    query     = renderQuery False . queryTextToQuery $ allParams
    allParams = params ++ [("api_key", Just key)]
    headers   = [("Accept", "application/json")]

--------------------------------------------------------------------------------
-- Build a URL and do an HTTP GET to TheMovieDB.
apiGET :: HC.Manager -> Key -> Path -> QueryText -> IO (Either Error Body)
apiGET manager key path params = do
  request  <- mkAPIRequest key path params
  response <- HC.httpLbs request manager
             
  return $ if statusIsSuccessful (HC.responseStatus response)
             then Right (HC.responseBody response)
             else Left (NetworkError . show $ HC.responseStatus response)
