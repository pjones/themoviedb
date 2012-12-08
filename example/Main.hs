{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}
module Main where

import Control.Monad (when)
import Data.Maybe (isNothing, fromJust)
import Data.Time (toGregorian, formatTime)
import Network.API.TheMovieDB
import Network.API.TheMovieDB.Util (loadConfig)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

-- Simple banner style printing of a 'Movie'
printMovieHeader :: Movie -> IO ()
printMovieHeader m = printf "%8d: %s (%s)\n" (movieID m) (movieTitle m) year
  where year = formatTime defaultTimeLocale "%Y" date
        date = releaseDate $ (movieReleaseDate m)

-- Print more detailed information for a 'Movie'.
printMovieDetails :: Movie -> IO ()
printMovieDetails m = do putStrLn $ moviePosterPath m
                         putStrLn $ "Popularity: " ++ show (moviePopularity m)
                         putStrLn $ unwords $ map genreName (movieGenres m)
                         putStrLn "-- "
                         putStrLn $ movieOverview m

-- Well, of course, it's main!
main :: IO ()
main = do args <- getArgs
          configM <- loadConfig
          when (isNothing configM) $ error "failed to load API key"
          let config = fromJust configM
          case args of
            ["search", query] -> do movies <- search config query
                                    mapM_ printMovieHeader movies

            ["fetch", mID]    -> do movie <- fetch config $ read mID
                                    printMovieHeader movie
                                    printMovieDetails movie

            _                 -> do putStrLn usage
                                    exitFailure

  where usage = "Usage: tmdb {search QUERY|fetch ID}"
