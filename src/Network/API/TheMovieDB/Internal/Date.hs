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
-- Utility type for working with release dates.
module Network.API.TheMovieDB.Internal.Date
  ( Date (..),
    parseDay,
    (.::),
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Text as Text
import Data.Time (Day (..), parseTimeM)
import Data.Time.Format (defaultTimeLocale)

-- | A simple type wrapper around 'Day' in order to parse a movie's
-- release date, which may be null or empty.
newtype Date = Date {day :: Maybe Day} deriving (Eq, Show)

-- | Aeson helper function to parse dates in TheMovieDB API.
parseDay :: Object -> Key -> Parser (Maybe Day)
parseDay v key = do
  m <- date
  return (m >>= day)
  where
    date :: Parser (Maybe Date)
    date = v .:? key <|> pure Nothing

-- | Infix alias for 'parseDay'.
(.::) :: Object -> Key -> Parser (Maybe Day)
(.::) = parseDay

-- | Parse release dates in JSON.
instance FromJSON Date where
  parseJSON Null = return (Date Nothing)
  parseJSON (String t)
    | Text.null t = return (Date Nothing)
    | otherwise = do
      d <- parseTimeM True defaultTimeLocale "%Y-%m-%d" (toString t)
      return $ Date (Just d)
  parseJSON v = typeMismatch "Date" v
