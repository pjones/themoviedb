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
-- Internal configuration information for TheMovieDB API.
module Network.API.TheMovieDB.Internal.Configuration
  ( Configuration (..),
    posterURLs,
  )
where

import Data.Aeson

-- | TheMovieDB API tries to preserve bandwidth by omitting
-- information (such as full URLs for poster images) from most of the
-- API calls.  Therefore in order to construct a complete URL for a
-- movie poster you'll need to use the 'config' function to retrieve
-- API configuration information.
--
-- A helper function is provided ('moviePosterURLs') that constructs a
-- list of all poster URLs given a 'Movie' and 'Configuration'.
--
-- According to the API documentation for TheMovieDB, you should cache
-- the 'Configuration' value and only request it every few days.
data Configuration = Configuration
  { -- | The base URL for images.
    cfgImageBaseURL :: Text,
    -- | Base URL for secure images.
    cfgImageSecBaseURL :: Text,
    -- | List of possible image sizes.
    cfgPosterSizes :: [Text]
  }
  deriving (Generic)

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \v ->
    Configuration
      <$> images v "base_url"
      <*> images v "secure_base_url"
      <*> imagesM v "poster_sizes" []
    where
      images v key = (v .: "images") >>= (.: key)
      imagesM v key def = (v .: "images") >>= (\x -> x .:? key .!= def)

instance ToJSON Configuration where
  toJSON c =
    object
      [ "images"
          .= object
            [ "base_url" .= cfgImageBaseURL c,
              "secure_base_url" .= cfgImageSecBaseURL c,
              "poster_sizes" .= cfgPosterSizes c
            ]
      ]

-- | Return a list of URLs for all possible posters.
posterURLs :: Configuration -> Text -> [Text]
posterURLs c p = [cfgImageBaseURL c <> size <> p | size <- cfgPosterSizes c]
