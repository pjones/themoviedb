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
import Control.Applicative
import Control.Exception
import Network.API.TheMovieDB.Internal.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

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
-- | Build a URL and do an HTTP GET to TheMovieDB.
apiGET :: HC.Manager -> Key -> Path -> QueryText -> IO (Either Error Body)
apiGET manager key path params = do
  request  <- mkAPIRequest key path params
  response <- catch (Right <$> HC.httpLbs request manager) httpError

  return $ case response of
    Left  e -> Left e
    Right r
      | statusIsSuccessful (HC.responseStatus r) -> Right (HC.responseBody r)
      | otherwise -> Left (ServiceError . show $ HC.responseStatus r)

  where
    httpError :: HC.HttpException -> IO (Either Error (HC.Response Body))
    httpError e = return $ case e of
      HC.StatusCodeException (Status 401 _) _ _ -> Left InvalidKeyError
      _                                         -> Left (HttpExceptionError e)
