{-# LANGUAGE OverloadedStrings #-}
-- | Module    : Network.TheMovieDB.API
-- Copyright   : (c) Peter Jones 2012
-- License     : MIT (see LICENSE.md)
-- Maintainer  : Peter Jones <pjones@pmade.com>
-- Stability   : alpha
--
module Network.TheMovieDB.API
       ( APIKey
       , APIError
       , SearchTerm
       , Movie(..)
       , searchErr
       , search
       ) where

import           Network.HTTP
import           Network.URI
import           Data.Aeson
import           Control.Applicative
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B

type APIKey          = String
type APIPath         = String
type APIQuery        = [(String, String)]
type SearchTerm      = String
type BodyContent     = B.ByteString

data APIError = NetworkError String
              | ParseError String
              deriving (Eq, Show)

data Movie = Movie
             { movieID    :: Int
             , movieTitle :: String
             } deriving (Eq, Show)

instance FromJSON Movie where
  parseJSON (Object v) =
    Movie <$> v .: "id"
          <*> v .: "original_title"
  parseJSON _ = empty

data SearchResults = SearchResults [Movie]

instance FromJSON SearchResults where
  parseJSON (Object v) =
    do movies <- (v .: "results") >>= parseJSON
       return $ SearchResults movies
  parseJSON _ = empty


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

-- Build a URL and do an HTTP GET.
apiGET :: APIKey -> APIPath -> [(String, String)] -> IO (Either APIError BodyContent)
apiGET key path params =
  do result <- simpleHTTP $ mkAPIRequest key path params
     case result of
       Left err -> return $ Left  $ NetworkError (show err)
       Right r  -> return $ Right $ rspBody r

--
moviesFromSearchJSON :: BodyContent -> Either APIError [Movie]
moviesFromSearchJSON body =
  case decode body of
    Nothing     -> Left $ ParseError ("failed to parse search results" ++ show body)
    Just (SearchResults m) -> Right m

--
searchErr :: APIKey -> SearchTerm -> IO (Either APIError [Movie])
searchErr key term =
  do result <- apiGET key "search/movie" [("query", term)]
     case result of
       Left err   -> return $ Left $ NetworkError $ show err
       Right body -> return $ moviesFromSearchJSON body

--
search :: APIKey -> SearchTerm -> IO [Movie]
search key term =
  do result <- searchErr key term
     case result of
       Left e  -> return []
       Right m -> return m
