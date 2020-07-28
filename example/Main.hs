{-

This file is part of the Haskell package themoviedb. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/themoviedb/LICENSE. No
part of themoviedb package, including this file, may be copied,
modified, propagated, or distributed except according to the terms
contained in the LICENSE file.

-}

module Main (main) where

import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Network.API.TheMovieDB
import System.Environment (getArgs)
import System.Environment (lookupEnv)
import Text.Printf (printf)

-- | Simple banner style printing of a 'Movie'.
printMovieHeader :: Movie -> IO ()
printMovieHeader m =
  printf "%8d: %s (%s)\n" (movieID m) (T.unpack $ movieTitle m) year
  where
    year = case movieReleaseDate m of
      Just d -> formatTime defaultTimeLocale "%Y" d
      Nothing -> "----"

-- | Simple banner style printing of a 'TV'.
printTVHeader :: TV -> IO ()
printTVHeader t =
  printf "%8d: %s (%s)\n" (tvID t) (T.unpack $ tvName t) year
  where
    year = case tvFirstAirDate t of
      Just d -> formatTime defaultTimeLocale "%Y" d
      Nothing -> "----"

-- | Print more detailed information for a 'Movie'.
printMovieDetails :: Movie -> IO ()
printMovieDetails m =
  do
    putStrLn $ "Popularity: " ++ show (moviePopularity m)
    putStrLn $ strJoin $ map genreName (movieGenres m)
    putStrLn "-- "
    putStrLn $ T.unpack (movieOverview m)
  where
    strJoin = T.unpack . T.intercalate ", "

-- | Print information about a season.
printSeason :: Season -> IO ()
printSeason s =
  do
    putStrLn $ "Season " ++ show (seasonNumber s) ++ " " ++ year
    mapM_ (putStrLn . episode) $ seasonEpisodes s
  where
    episode e =
      "\tE" ++ show (episodeNumber e) ++ " "
        ++ T.unpack (episodeName e)
    year = case seasonAirDate s of
      Just d -> formatTime defaultTimeLocale "%Y" d
      Nothing -> "----"

-- | Search for movies with a query string.
searchAndListMovies :: Text -> TheMovieDB ()
searchAndListMovies query = do
  movies <- searchMovies query
  liftIO $ mapM_ printMovieHeader movies

-- | Find a specific movie given its ID.
fetchAndPrintMovie :: ItemID -> TheMovieDB ()
fetchAndPrintMovie mid = do
  cfg <- config
  movie <- fetchMovie mid

  liftIO $ do
    printMovieHeader movie
    mapM_ (putStrLn . T.unpack) (moviePosterURLs cfg movie)
    printMovieDetails movie

-- | Search for TV series using a query string.
searchAndListTV :: Text -> TheMovieDB ()
searchAndListTV query = do
  tvs <- searchTV query
  liftIO $ mapM_ printTVHeader tvs

-- | Find a specific TV series given its ID.
fetchAndPrintTV :: ItemID -> TheMovieDB ()
fetchAndPrintTV tid = do
  tv <- fetchTV tid
  liftIO $ printTVHeader tv
  liftIO $ mapM_ printSeason (tvSeasons tv)

fetchAndPrintSeason :: ItemID -> Int -> TheMovieDB ()
fetchAndPrintSeason t s = do
  season <- fetchTVSeason t s
  liftIO $ printSeason season

fetchAndPrintFullTV :: ItemID -> TheMovieDB ()
fetchAndPrintFullTV t = do
  tv <- fetchFullTVSeries t
  liftIO $ printTVHeader tv
  liftIO $ mapM_ printSeason (tvSeasons tv)

-- | Low budget command line parsing and dispatch.
main :: IO ()
main = do
  args <- getArgs
  lang <- lookupEnv "TMDB_LANG"

  let key = maybe T.empty T.pack (listToMaybe args)
      settings = Settings key (toText <$> lang)

  result <- runTheMovieDB settings $
    case args of
      [_, "search", query] -> searchAndListMovies (T.pack query)
      [_, "fetch", mid] -> itemID fetchAndPrintMovie mid
      [_, "tsearch", query] -> searchAndListTV (T.pack query)
      [_, "tfetch", tid] -> itemID fetchAndPrintTV tid
      [_, "season", t, s] -> itemID (\t -> itemID (fetchAndPrintSeason t) s) t
      [_, "tfull", t] -> itemID fetchAndPrintFullTV t
      _ -> liftIO (putStrLn usage >> exitFailure)

  case result of
    Left err -> print err >> exitFailure
    Right _ -> exitSuccess
  where
    itemID :: MonadIO m => (ItemID -> m a) -> String -> m a
    itemID f str =
      case readMaybe str of
        Nothing -> die ("invalid ID: " <> str)
        Just n -> f n
    usage =
      "Usage: tmdb key {search|fetch|tsearch|tfetch|season|tfull}\n\n"
        ++ "Description:\n"
        ++ "       search: search TheMovieDB\n"
        ++ "        fetch: fetch info about one movie\n"
        ++ "      tsearch: search for TV series\n"
        ++ "       tfetch: fetch TV series by ID\n"
        ++ "       season: fetch one season by TV ID and season num\n"
        ++ "        tfull: pull everything about TV series by ID\n"
        ++ "Examples:\n"
        ++ "       tmdb KEY search \"back to the future\"\n"
        ++ "       tmdb KEY fetch 105\n"
        ++ "       tmdb KEY tsearch Firefly\n"
        ++ "       tmdb KEY tfetch 1437\n"
        ++ "       tmdb KEY season 1437 1\n"
        ++ "       tmdb KEY tfull 1437\n"
