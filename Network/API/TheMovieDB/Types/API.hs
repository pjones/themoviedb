{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Types.API
       ( Key
       , Body
       , Response
       , Path
       , Params
       , IOFunc
       , Error(..)
       ) where

import qualified Data.ByteString.Lazy as B

-- | Type for the API key issued by TheMovieDB.
type Key = String

-- | Possible errors returned by the API.
data Error
  = NetworkError String -- ^ Network or HTTP error.
  | ParseError   String -- ^ Invalid or error response from the API.
  deriving (Eq, Show)

-- Internal types.           
type Path     = String
type Params   = [(String, String)]
type Body     = B.ByteString
type Response = Either Error Body
type IOFunc   = Key -> Path -> Params -> IO Response           
