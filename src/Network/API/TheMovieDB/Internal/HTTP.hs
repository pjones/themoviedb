-- |
--
-- Copyright:
--   This file is part of the package themoviedb.  It is subject to
--   the license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/themoviedb
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: MIT
--
-- Simple interface for fetching JSON files from the API via HTTP.
module Network.API.TheMovieDB.Internal.HTTP
  ( apiBaseURL,
    defaultImagePrefix,
    apiGET,
  )
where

import Control.Exception
import Network.API.TheMovieDB.Internal.Settings
import Network.API.TheMovieDB.Internal.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types

-- | The base URL for the version of the API we're using.
apiBaseURL :: String
apiBaseURL = "https://api.themoviedb.org/3/"

-- | The default image URL prefix.
defaultImagePrefix :: Text
defaultImagePrefix = "http://image.tmdb.org/t/p/"

-- | Build a HTTP request that can be used to access the API.
mkAPIRequest :: Settings -> Path -> QueryText -> IO HC.Request
mkAPIRequest Settings {..} path params = do
  req <- HC.parseRequest (apiBaseURL ++ path)

  return $
    req
      { HC.queryString = query,
        HC.requestHeaders = headers
      }
  where
    query = renderQuery False . queryTextToQuery $ allParams
    headers = [("Accept", "application/json")]
    allParams =
      params
        ++ ( second Just
               <$> catMaybes
                 [ Just ("api_key", tmdbKey),
                   ("language",) <$> tmdbLanguage
                 ]
           )

-- | Build a URL and do an HTTP GET to TheMovieDB.
apiGET :: HC.Manager -> Settings -> Path -> QueryText -> IO (Either Error Body)
apiGET manager settings path params = do
  request <- mkAPIRequest settings path params
  response <- catch (Right <$> HC.httpLbs request manager) httpError

  return $ case response of
    Left e -> Left e
    Right r
      | statusIsSuccessful (HC.responseStatus r) -> Right (HC.responseBody r)
      | otherwise -> Left (ServiceError . show $ HC.responseStatus r)
  where
    -- This should only be called for non-200 codes now.
    httpError :: HC.HttpException -> IO (Either Error (HC.Response Body))
    httpError _ = return $ Left InvalidKeyError
