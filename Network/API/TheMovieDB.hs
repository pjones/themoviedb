{-# LANGUAGE OverloadedStrings #-}

{-|
     Module: Network.API.TheMovieDB
  Copyright: (c) 2012 Peter Jones
    License: MIT
  Stability: experimental
Portability: portable
-}
module Network.API.TheMovieDB
       ( APIKey
       , APIError
       , SearchTerm
       , Movie(..)
       , searchErr
       , search
       ) where

import Network.API.TheMovieDB.Types
import Network.API.TheMovieDB.Search
