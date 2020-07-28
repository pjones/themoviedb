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
module TestHelper
  ( fakeTMDB,
    fileRequest,
    fakeNetworkError,
    mkDay,
  )
where

import Data.Time (Day (..), fromGregorian)
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Internal.Types
import Test.Tasty.HUnit

fakeTMDB :: FilePath -> TheMovieDB a -> IO a
fakeTMDB path m = do
  result <- runTheMovieDBWithRequestFunction (fileRequest path) m

  case result of
    Left e -> assertFailure (show e) >> fail (show e)
    Right a -> return a

fileRequest :: FilePath -> RequestFunction
fileRequest path _ _ = Right <$> readFileLBS path

fakeNetworkError :: RequestFunction
fakeNetworkError _ _ = return $ Left $ ServiceError "fake outage"

mkDay :: Integer -> Int -> Int -> Maybe Day
mkDay y m d = Just (fromGregorian y m d)
