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
module Network.API.TheMovieDB.Types.Genre
  ( Genre (..),
  )
where

import Data.Aeson
import Network.API.TheMovieDB.Internal.Types

-- | Metadata for a genre.
data Genre = Genre
  { -- | TheMovieDB unique ID.
    genreID :: ItemID,
    -- | The name of the genre.
    genreName :: Text
  }
  deriving (Eq, Show)

instance FromJSON Genre where
  parseJSON = withObject "Genre" $ \v ->
    Genre
      <$> v .: "id"
      <*> v .: "name"
