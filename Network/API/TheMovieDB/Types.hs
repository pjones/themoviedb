{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Network.TheMovieDB.API.Types
-- Copyright   : (c) Peter Jones 2012
-- License     : MIT (see LICENSE.md)
-- Maintainer  : Peter Jones <pjones@pmade.com>
-- Stability   : alpha
module Network.TheMovieDB.API.Types
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
