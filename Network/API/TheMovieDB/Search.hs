{-# LANGUAGE OverloadedStrings #-}
module Network.TheMovieDB.API.Search
       ( searchErr
       , search
       ) where

import Network.API.TheMovieDB.Types
import Network.API.TheMovieDB.HTTP

type SearchTerm = String
data SearchResults = SearchResults [Movie]

instance FromJSON SearchResults where
  parseJSON (Object v) =
    do movies <- (v .: "results") >>= parseJSON
       return $ SearchResults movies
  parseJSON _ = empty

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
