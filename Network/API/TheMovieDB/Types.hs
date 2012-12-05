{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Types
       ( APIKey
       , APIError(..)
       , Movie(..)
       ) where

import Data.Aeson
import Control.Applicative

type APIKey = String

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
