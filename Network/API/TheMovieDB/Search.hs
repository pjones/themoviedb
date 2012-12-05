{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Search
       ( SearchTerm
       , searchErr
       , search
       ) where

import Network.API.TheMovieDB.Types
import Network.API.TheMovieDB.HTTP
import Data.Aeson
import Control.Applicative

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
