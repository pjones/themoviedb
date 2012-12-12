{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Types.Configuration
       ( Configuration(..)
       ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)

-- | TheMovieDB API tries to preserve bandwidth by omitting
-- information like full URLs for images from most of the API calls
-- which requires that you fetch such information separately.
data Configuration = Configuration
  { cfgImageBaseURL    :: String   -- ^ The base URL for images.
  , cfgImageSecBaseURL :: String   -- ^ Base URL for secure images.
  , cfgPosterSizes     :: [String] -- ^ List of possible image sizes.
  }

instance FromJSON Configuration where
  parseJSON (Object v) = do
    Configuration <$> images  "base_url"
                  <*> images  "secure_base_url"
                  <*> imagesM "poster_sizes" []
    where images key      = (v .: "images") >>= (\x -> x .:  key)
          imagesM key def = (v .: "images") >>= (\x -> x .:? key .!= def)
  parseJSON _ = empty
