{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Network.API.TheMovieDB.Internal.TheMovieDB
       ( TheMovieDB
       , RequestFunction
       , runRequest
       , runTheMovieDB
       , runTheMovieDBWithManager
       , runTheMovieDBWithRequestFunction
       ) where

--------------------------------------------------------------------------------
-- import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Monad.Reader
import Network.API.TheMovieDB.Internal.HTTP
import Network.API.TheMovieDB.Internal.Types
import Network.HTTP.Client (Manager, withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types

--------------------------------------------------------------------------------
type RequestFunction = Path -> QueryText -> TheMovieDB (Either Error Body)

--------------------------------------------------------------------------------
newtype TheMovieDB a = TheMovieDB {unTMDB :: ReaderT RequestFunction IO a}
                       deriving (Functor, Applicative, Monad, MonadIO)

--------------------------------------------------------------------------------
runRequest :: Path -> QueryText -> TheMovieDB (Either Error Body)
runRequest path params = TheMovieDB $ ask >>= \f -> unTMDB (f path params)

--------------------------------------------------------------------------------
internalReqFunc :: Manager -> Key -> RequestFunction                                                    
internalReqFunc m k p q = liftIO $ apiGET m k p q

--------------------------------------------------------------------------------
runTheMovieDB :: Key
              -> TheMovieDB a
              -> IO a
runTheMovieDB k t =
  withManager tlsManagerSettings (\m -> runTheMovieDBWithManager m k t)

--------------------------------------------------------------------------------
runTheMovieDBWithManager :: Manager
                         -> Key
                         -> TheMovieDB a
                         -> IO a
runTheMovieDBWithManager m k t =
  runTheMovieDBWithRequestFunction (internalReqFunc m k) t

--------------------------------------------------------------------------------
runTheMovieDBWithRequestFunction :: RequestFunction
                                 -> TheMovieDB a
                                 -> IO a
runTheMovieDBWithRequestFunction f t = runReaderT (unTMDB t) f
