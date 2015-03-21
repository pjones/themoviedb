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
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)

--------------------------------------------------------------------------------
-- | Type for the API key issued by TheMovieDB.
type Key = Text

--------------------------------------------------------------------------------
-- Internal types.
type Path = String
type Body = ByteString

--------------------------------------------------------------------------------
-- | Possible errors returned by the API.
data Error = NetworkError String
             -- ^ Network or HTTP error.
             
           | ParseError String (Maybe ByteString)
             -- ^ Invalid or error response from the API.
             
           deriving (Eq, Show)

