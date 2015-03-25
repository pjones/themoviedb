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
-- | Internal configuration information for TheMovieDB API.
module Network.API.TheMovieDB.Internal.Configuration
       ( Configuration (..)
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson

--------------------------------------------------------------------------------
-- | TheMovieDB API tries to preserve bandwidth by omitting
-- information (such as full URLs for poster images) from most of the
-- API calls.  Therefore in order to construct a complete URL for a
-- movie poster you'll need to use the 'config' function to retrieve
-- API configuration information.
--
-- A helper function is provided ('moviePosterURLs') that constructs a
-- list of all poster URLs given a 'Movie' and 'Configuration'.
data Configuration = Configuration
  { cfgImageBaseURL    :: String   -- ^ The base URL for images.
  , cfgImageSecBaseURL :: String   -- ^ Base URL for secure images.
  , cfgPosterSizes     :: [String] -- ^ List of possible image sizes.
  }

--------------------------------------------------------------------------------
instance FromJSON Configuration where
  parseJSON (Object v) =
    Configuration <$> images  "base_url"
                  <*> images  "secure_base_url"
                  <*> imagesM "poster_sizes" []
    where images key      = (v .: "images") >>= (.:  key)
          imagesM key def = (v .: "images") >>= (\x -> x .:? key .!= def)
  parseJSON _ = empty
