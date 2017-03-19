{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Utility type for working with release dates.
module Network.API.TheMovieDB.Internal.Date
       ( Date (..)
       , parseDay
       , (.::)
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day(..), parseTimeM)
import Data.Time.Locale.Compat (defaultTimeLocale)

--------------------------------------------------------------------------------
-- | A simple type wrapper around 'Day' in order to parse a movie's
-- release date, which may be null or empty.
newtype Date = Date {day :: Maybe Day} deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Aeson helper function to parse dates in TheMovieDB API.
parseDay :: Object -> Text -> Parser (Maybe Day)
parseDay v key =
  do m <- date
     return $ case m of
       Nothing -> Nothing
       Just d  -> day d
  where
    date :: Parser (Maybe Date)
    date = v .:? key <|> pure Nothing

--------------------------------------------------------------------------------
(.::) :: Object -> Text -> Parser (Maybe Day)
(.::) = parseDay

--------------------------------------------------------------------------------
-- | Parse release dates in JSON.
instance FromJSON Date where
  parseJSON Null = return (Date Nothing)
  parseJSON (String t)
    | T.null t  = return (Date Nothing)
    | otherwise = do d <- parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)
                     return $ Date (Just d)

  parseJSON v = typeMismatch "Date" v
