{-# OPTIONS_GHC -Wwarn #-} -- Kludge to not error out on withManager deprecation.
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
       , getAndParse
       , tmdbError
       , runTheMovieDB
       , runTheMovieDBWithManager
       , runTheMovieDBWithRequestFunction
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Aeson
import Network.API.TheMovieDB.Internal.HTTP
import Network.API.TheMovieDB.Internal.Types
import Network.HTTP.Client (Manager, withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | The type for functions that make requests to the API (or pretend
-- to make a request for testing purposes).
type RequestFunction = (Path -> QueryText -> IO (Either Error Body))

--------------------------------------------------------------------------------
-- | Result type for operations involving TheMovieDB API.
newtype TheMovieDB a =
  TheMovieDB {unTMDB :: ReaderT RequestFunction (EitherT Error IO) a}
  deriving (Functor, Applicative, Monad, MonadIO)

--------------------------------------------------------------------------------
-- | Helper function for making a request using the request function
-- stashed away in the reader monad.
runRequest :: Path -> QueryText -> TheMovieDB Body
runRequest path params = TheMovieDB $ do
  func   <- ask
  result <- liftIO (func path params)
  lift (hoistEither result)

--------------------------------------------------------------------------------
-- | Helper function to preform an HTTP GET and decode the JSON result.
getAndParse :: FromJSON a => Path -> QueryText -> TheMovieDB a
getAndParse path params = do
  body <- runRequest path params

  case eitherDecode body of
    Left  e -> tmdbError $ ResponseParseError ("bad JSON: " ++ e) (Just body)
    Right a -> return a

--------------------------------------------------------------------------------
-- | Create a 'TheMovieDB' value representing an error.
tmdbError :: Error -> TheMovieDB a
tmdbError = TheMovieDB . lift . left

--------------------------------------------------------------------------------
-- | Execute requests for TheMovieDB with the given API key and produce
-- either an error or a result.
--
-- This version creates a temporary 'Manager' using
-- 'tlsManagerSettings'.  If you want to use an existing 'Manager' you
-- should use 'runTheMovieDBWithManager' instead.
runTheMovieDB
  :: Key                        -- ^ The API key to include in all requests.
  -> TheMovieDB a               -- ^ The API calls to make.
  -> IO (Either Error a)        -- ^ Response or error.
runTheMovieDB k t = -- TODO: replace withManager with newManager.
  withManager tlsManagerSettings (\m -> runTheMovieDBWithManager m k t)

--------------------------------------------------------------------------------
-- | Execute requests for TheMovieDB with the given API key and produce
-- either an error or a result.
--
-- This version allows you to provide a 'Manager' value which should
-- have been created to allow TLS requests (e.g., with 'tlsManagerSettings').
runTheMovieDBWithManager
  :: Manager                    -- ^ The 'Manager' to use.
  -> Key                        -- ^ The API key to include in all requests.
  -> TheMovieDB a               -- ^ The API calls to make.
  -> IO (Either Error a)        -- ^ Response or error.
runTheMovieDBWithManager m k = runTheMovieDBWithRequestFunction (apiGET m k)

--------------------------------------------------------------------------------
-- | Low-level interface for executing a 'TheMovieDB' using the given
-- request function.
runTheMovieDBWithRequestFunction
  :: RequestFunction            -- ^ The request function to use.
  -> TheMovieDB a               -- ^ The API calls to make.
  -> IO (Either Error a)        -- ^ Response.
runTheMovieDBWithRequestFunction f t = runEitherT $ runReaderT (unTMDB t) f
