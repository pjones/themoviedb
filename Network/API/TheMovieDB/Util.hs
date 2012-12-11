{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Network.API.TheMovieDB.Util
       ( loadKey
       , loadContext
       ) where

import Control.Monad (liftM, msum)
import Data.Char (isSpace)
import Network.API.TheMovieDB.HTTP
import Network.API.TheMovieDB.Types
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
import System.Posix.User

-- Fetch the value of an environment variable without an exception.
getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe n = env `catch` err
  where env   = liftM Just $ getEnv n
        err e = if isDoesNotExistError e
                  then return Nothing
                  else ioError e

-- Expand @~@ at the front of a file path.
expandFile :: FilePath -> IO FilePath
expandFile ('~':rest) = do userID <- getEffectiveUserID
                           entry  <- getUserEntryForID userID
                           return  $ homeDirectory entry ++ rest
expandFile dir        = return dir

-- Return the contents of a file if it exists, otherwise Nothing.
readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe n = do realName <- expandFile n
                     exists   <- fileExist realName
                     if exists
                       then liftM Just (readFirstLine realName)
                       else return Nothing
  where readFirstLine = liftM skipSpace . readFile
        skipSpace     = filter $ not . isSpace


-- | Fetch an API 'Key' from the first of:
--
--     * @TMDB_KEY@ environment variable
--
--     * @XDG_CONFIG_HOME/tmdbkey@ file (where XDG_CONFIG_HOME is
--       an environment variable)
--
--     * @~\/.config\/tmdbkey@ file
--
--     * @~/.tmdbkey@ file
--
--   If the key can't be loaded from any of those places the result
--   will be Nothing.
loadKey :: IO (Maybe Key)
loadKey = liftM msum . sequence $ [env, xdgConfig, config, home]
  where env       = getEnvMaybe "TMDB_KEY"
        config    = readFileMaybe "~/.config/tmdbkey"
        home      = readFileMaybe "~/.tmdbkey"
        xdgConfig = do dir <- getEnvMaybe "XDG_CONFIG_HOME"
                       case dir of
                         Nothing -> return Nothing
                         Just d  -> readFileMaybe (d ++ "/tmdbkey")

-- | Uses 'loadKey' to fetch an API 'Key' and wrap it into a default
--   'Context' using 'mkContext'.
loadContext :: IO (Maybe Context)
loadContext = do key <- loadKey
                 return $ maybe Nothing (Just . mkContext) key
