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
-- Simple types and synonyms, mostly to make the type signatures
-- easier to read.
module Network.API.TheMovieDB.Internal.Types
  ( ItemID,
    Key,
    Body,
    Path,
    Error (..),
  )
where

import Network.HTTP.Client (HttpException)

-- | Type to represent IDs used by the API.
type ItemID = Int

-- | Type for the API key issued by TheMovieDB.
type Key = Text

-- | URL path.
type Path = String

-- | HTTP body.
type Body = LByteString

-- | Possible errors returned by the API.
data Error
  = -- | Missing or invalid API key.  Make sure you are using
    -- a valid API key issued by
    -- <https://www.themoviedb.org/faq/api>.
    InvalidKeyError
  | -- | An exception relating to HTTP was thrown while
    -- interacting with the API.
    HttpExceptionError HttpException
  | -- | The HTTP interaction with the API service did not
    -- result in a successful response.  Information about
    -- the failure is encoded in the String.
    ServiceError String
  | -- | Invalid or error response from the API.
    ResponseParseError String (Maybe LByteString)
  deriving (Show)
