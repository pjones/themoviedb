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
module Network.API.TheMovieDB.Internal.Settings
  ( Settings (..),
    defaultSettings,
  )
where

import qualified Data.Aeson as Aeson
import Network.API.TheMovieDB.Internal.Types

-- | Settings used by this library.
data Settings = Settings
  { -- | The API key to use.
    tmdbKey :: Key,
    -- | Optional ISO 639-1 language code to send with every request.
    tmdbLanguage :: Maybe LanguageCode
  }

instance Aeson.FromJSON Settings where
  parseJSON = Aeson.withObject "Settings" $ \v ->
    Settings
      <$> v Aeson..: "key"
      <*> v Aeson..: "lang"

instance Aeson.ToJSON Settings where
  toJSON Settings {..} =
    Aeson.object
      [ "key" Aeson..= tmdbKey,
        "lang" Aeson..= tmdbLanguage
      ]

-- | Default settings.
defaultSettings :: Key -> Settings
defaultSettings key =
  Settings
    { tmdbKey = key,
      tmdbLanguage = Nothing
    }
