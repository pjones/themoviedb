{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (formatTime)
import Network.API.TheMovieDB
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- | Simple banner style printing of a 'Movie'.
printMovieHeader :: Movie -> IO ()
printMovieHeader m =
  printf "%8d: %s (%s)\n" (movieID m) (T.unpack $ movieTitle m) year
  where year = case movieReleaseDate m of
                 Just d  -> formatTime defaultTimeLocale "%Y" d
                 Nothing -> "----"

--------------------------------------------------------------------------------
-- | Print more detailed information for a 'Movie'.
printMovieDetails :: Movie -> IO ()
printMovieDetails m =
  do putStrLn $ "Popularity: " ++ show (moviePopularity m)
     putStrLn $ strJoin $ map genreName (movieGenres m)
     putStrLn "-- "
     putStrLn $ T.unpack (movieOverview m)
  where strJoin = T.unpack . T.intercalate ", "

--------------------------------------------------------------------------------
-- | Search for movies with a query string.
searchAndListMovies :: Text -> TheMovieDB ()
searchAndListMovies query = do
  movies <- searchMovies query
  liftIO $ mapM_ printMovieHeader movies

--------------------------------------------------------------------------------
-- | Find a specific movie given its ID.
fetchAndPrintMovie :: MovieID -> TheMovieDB ()
fetchAndPrintMovie mid = do
  cfg <- config
  movie <- fetchMovie mid

  liftIO $ do
    printMovieHeader movie
    mapM_ (putStrLn . T.unpack) (moviePosterURLs cfg movie)
    printMovieDetails movie

--------------------------------------------------------------------------------
-- | Low budget command line parsing and dispatch.
main :: IO ()
main = do
  args <- getArgs
  let key = maybe T.empty T.pack (listToMaybe args)

  result <- runTheMovieDB key $
    case args of
      [_, "search", query] -> searchAndListMovies (T.pack query)
      [_, "fetch", mid]    -> fetchAndPrintMovie (read mid)
      _                    -> liftIO (putStrLn usage >> exitFailure)

  case result of
    Left err -> putStrLn (show err) >> exitFailure
    Right _  -> exitSuccess

  where usage = "Usage: tmdb key {search QUERY|fetch ID}\n\n" ++
                "Description:\n" ++
                "       search: search TheMovieDB\n" ++
                "       fetch:  fetch info about one movie\n" ++
                 "Examples:\n" ++
                "       tmdb KEY search \"back to the future\"\n" ++
                "       tmdb KEY fetch 105"
