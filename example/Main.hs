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
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Data.Time (toGregorian, formatTime)
import Network.API.TheMovieDB
import Network.API.TheMovieDB.Util (loadContext)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

-- Simple banner style printing of a 'Movie'
printMovieHeader :: Movie -> IO ()
printMovieHeader m =
  printf "%8d: %s (%s)\n" (movieID m) (movieTitle m) year
  where year = formatTime defaultTimeLocale "%Y" $ movieReleaseDate m

-- Print more detailed information for a 'Movie'.
printMovieDetails :: Movie -> IO ()
printMovieDetails m =
  do putStrLn $ "Popularity: " ++ show (moviePopularity m)
     putStrLn $ strJoin $ map genreName (movieGenres m)
     putStrLn "-- "
     putStrLn $ movieOverview m
  where strJoin = intercalate ", "

-- Well, of course, it's main!
main :: IO ()
main = do args <- getArgs
          contextM <- loadContext
          when (isNothing contextM) $ error "failed to load API key"
          let context = fromJust contextM
          cfg <- config context
          case args of
            ["key"]           -> do putStrLn $ apiKey context

            ["search", query] -> do movies <- search context query
                                    mapM_ printMovieHeader movies

            ["fetch", mID]    -> do movie <- fetch context (read mID)
                                    printMovieHeader movie
                                    mapM_ putStrLn $ moviePosterURLs cfg movie
                                    printMovieDetails movie

            _                 -> do putStrLn usage
                                    exitFailure

  where usage = "Usage: tmdb {search QUERY|fetch ID|key}\n\n" ++
                "Description:\n" ++
                "       search: search TheMovieDB\n" ++
                "       fetch:  fetch info about one movie\n" ++
                "       key:    output your API key\n\n" ++
                "Examples:\n" ++
                "       tmdb search \"back to the future\"\n" ++
                "       tmdb fetch 105"
