{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Types.ReleaseDate (ReleaseDate(..)) where
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (unpack)
import Data.Time (parseTime, Day(..))
import System.Locale (defaultTimeLocale)

-- Type wrapper for Day to parse a movie's release date.
newtype ReleaseDate = ReleaseDate
  {releaseDate :: Day} deriving (Eq, Show)

-- Parse release dates in JSON.
instance FromJSON ReleaseDate where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%Y-%m-%d" (unpack t) of
      Just d -> return $ ReleaseDate d
      _      -> fail "could not parse release_date"
  parseJSON v = typeMismatch "ReleaseDate" v
