{-# LANGUAGE OverloadedStrings,  DeriveGeneric #-}

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
       , posterURLs
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Data.Binary
import Data.Monoid
import Data.Text (Text)
import Data.Text.Binary ()
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
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
-- Therefore, it is an instance of the 'Binary' class so it can be
-- serialized to and from a cache file on disk.
--
-- Alternatively, the 'FromJSON' and 'ToJSON' instances can be used to
-- cache the 'Configuration' value.
data Configuration = Configuration
  { cfgImageBaseURL    :: Text   -- ^ The base URL for images.
  , cfgImageSecBaseURL :: Text   -- ^ Base URL for secure images.
  , cfgPosterSizes     :: [Text] -- ^ List of possible image sizes.
  } deriving (Generic)

--------------------------------------------------------------------------------
instance Binary Configuration

--------------------------------------------------------------------------------
instance FromJSON Configuration where
  parseJSON (Object v) =
    Configuration <$> images  "base_url"
                  <*> images  "secure_base_url"
                  <*> imagesM "poster_sizes" []
    where images key      = (v .: "images") >>= (.:  key)
          imagesM key def = (v .: "images") >>= (\x -> x .:? key .!= def)
  parseJSON _ = empty

--------------------------------------------------------------------------------
instance ToJSON Configuration where
  toJSON c = object [ "images" .= object
    [ "base_url"        .= cfgImageBaseURL c
    , "secure_base_url" .= cfgImageSecBaseURL c
    , "poster_sizes"    .= cfgPosterSizes c
    ]]


--------------------------------------------------------------------------------
-- | Return a list of URLs for all possible posters posters.
posterURLs :: Configuration -> Text -> [Text]
posterURLs c p = [cfgImageBaseURL c <> size <> p | size <- cfgPosterSizes c]
