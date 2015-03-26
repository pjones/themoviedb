{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.API.TheMovieDB.Internal.Types
       ( Key
       , Body
       , Path
       , Error (..)
       ) where

--------------------------------------------------------------------------------
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Client (HttpException)

--------------------------------------------------------------------------------
-- | Type for the API key issued by TheMovieDB.
type Key = Text

--------------------------------------------------------------------------------
-- Internal types.
type Path = String
type Body = ByteString

--------------------------------------------------------------------------------
-- | Possible errors returned by the API.
data Error = InvalidKeyError
             -- ^ Missing or invalid API key.  Make sure you are using
             -- a valid API key issued by
             -- <https://www.themoviedb.org/faq/api>.

           | HttpExceptionError HttpException
             -- ^ An exception relating to HTTP was thrown while
             -- interacting with the API.
             
           | ServiceError String
             -- ^ The HTTP interaction with the API service did not
             -- result in a successful response.  Information about
             -- the failure is encoded in the String.
             
           | ResponseParseError String (Maybe ByteString)
             -- ^ Invalid or error response from the API.
             
           deriving (Show)

