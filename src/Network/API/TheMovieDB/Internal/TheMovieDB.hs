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
       , apiError
       , runTheMovieDB
       , runTheMovieDBWithManager
       , runTheMovieDBWithRequestFunction
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Network.API.TheMovieDB.Internal.HTTP
import Network.API.TheMovieDB.Internal.Types
import Network.HTTP.Client (Manager, withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types

--------------------------------------------------------------------------------
-- | The type for functions that make requests to the API (or pretend
-- to make a request for testing purposes).
type RequestFunction = (Path -> QueryText -> IO (Either Error Body))

--------------------------------------------------------------------------------
-- | Result type for operations involving TheMovieDB API.
newtype TheMovieDB a =
  TheMovieDB {unTMDB :: ReaderT RequestFunction (EitherT Error IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader RequestFunction)

--------------------------------------------------------------------------------
-- | Helper function for making a request using the request function
-- stashed away in the reader monad.
runRequest :: Path -> QueryText -> TheMovieDB Body
runRequest path params = do
  func   <- ask
  result <- liftIO (func path params)
  TheMovieDB $ lift $ hoistEither result

--------------------------------------------------------------------------------
apiError :: Error -> TheMovieDB a  
apiError = TheMovieDB . lift . left

--------------------------------------------------------------------------------
runTheMovieDB
  :: Key
  -> TheMovieDB a
  -> IO (Either Error a)
runTheMovieDB k t =
  withManager tlsManagerSettings (\m -> runTheMovieDBWithManager m k t)

--------------------------------------------------------------------------------
runTheMovieDBWithManager
  :: Manager
  -> Key
  -> TheMovieDB a
  -> IO (Either Error a)
runTheMovieDBWithManager m k t =
  runTheMovieDBWithRequestFunction (apiGET m k) t

--------------------------------------------------------------------------------
-- | Low-level interface for executing a 'TheMovieDB' using the given
-- request function.
runTheMovieDBWithRequestFunction
  :: RequestFunction            -- ^ The request function to use.
  -> TheMovieDB a               -- ^ The API calls to make.
  -> IO (Either Error a)        -- ^ Response.
runTheMovieDBWithRequestFunction f t = runEitherT $ runReaderT (unTMDB t) f
