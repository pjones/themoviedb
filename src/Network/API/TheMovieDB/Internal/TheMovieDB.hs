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
module Network.API.TheMovieDB.Internal.TheMovieDB
  ( TheMovieDB,
    RequestFunction,
    getAndParse,
    tmdbError,
    runTheMovieDB,
    runTheMovieDBWithManager,
    runTheMovieDBWithRequestFunction,
  )
where

import Control.Monad.Except
import Data.Aeson
import Network.API.TheMovieDB.Internal.HTTP
import Network.API.TheMovieDB.Internal.Settings
import Network.API.TheMovieDB.Internal.Types
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types

-- | The type for functions that make requests to the API (or pretend
-- to make a request for testing purposes).
type RequestFunction = (Path -> QueryText -> IO (Either Error Body))

-- | Result type for operations involving TheMovieDB API.
newtype TheMovieDB a = TheMovieDB
  {unTMDB :: ReaderT RequestFunction (ExceptT Error IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Helper function for making a request using the request function
-- stashed away in the reader monad.
runRequest :: Path -> QueryText -> TheMovieDB Body
runRequest path params = TheMovieDB $ do
  func <- ask
  lift (ExceptT $ liftIO (func path params))

-- | Helper function to preform an HTTP GET and decode the JSON result.
getAndParse :: FromJSON a => Path -> QueryText -> TheMovieDB a
getAndParse path params = do
  body <- runRequest path params

  case eitherDecode body of
    Left e -> tmdbError $ ResponseParseError ("bad JSON: " ++ e) (Just body)
    Right a -> return a

-- | Create a 'TheMovieDB' value representing an error.
tmdbError :: Error -> TheMovieDB a
tmdbError = TheMovieDB . throwError

-- | Execute requests for TheMovieDB with the given API key and produce
-- either an error or a result.
--
-- This version creates a temporary 'Manager' using
-- 'tlsManagerSettings'.  If you want to use an existing 'Manager' you
-- should use 'runTheMovieDBWithManager' instead.
runTheMovieDB ::
  -- | Library settings.
  Settings ->
  -- | The API calls to make.
  TheMovieDB a ->
  -- | Response or error.
  IO (Either Error a)
runTheMovieDB s t = do
  m <- newManager tlsManagerSettings
  runTheMovieDBWithManager m s t

-- | Execute requests for TheMovieDB with the given API key and produce
-- either an error or a result.
--
-- This version allows you to provide a 'Manager' value which should
-- have been created to allow TLS requests (e.g., with 'tlsManagerSettings').
runTheMovieDBWithManager ::
  -- | The 'Manager' to use.
  Manager ->
  -- | Library settings.
  Settings ->
  -- | The API calls to make.
  TheMovieDB a ->
  -- | Response or error.
  IO (Either Error a)
runTheMovieDBWithManager m s = runTheMovieDBWithRequestFunction (apiGET m s)

-- | Low-level interface for executing a 'TheMovieDB' using the given
-- request function.
runTheMovieDBWithRequestFunction ::
  -- | The request function to use.
  RequestFunction ->
  -- | The API calls to make.
  TheMovieDB a ->
  -- | Response.
  IO (Either Error a)
runTheMovieDBWithRequestFunction f t = runExceptT $ runReaderT (unTMDB t) f
