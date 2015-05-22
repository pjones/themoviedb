--------------------------------------------------------------------------------
module TestHelper
       ( fakeTMDB
       , fileRequest
       , fakeNetworkError
       , mkDay
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Data.Time (Day (..), fromGregorian)
import Network.API.TheMovieDB.Internal.TheMovieDB
import Network.API.TheMovieDB.Internal.Types
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
fakeTMDB :: FilePath -> TheMovieDB a -> IO a
fakeTMDB path m = do
  result <- runTheMovieDBWithRequestFunction (fileRequest path) m

  case result of
    Left  e -> assertFailure (show e) >> fail (show e)
    Right a -> return a

--------------------------------------------------------------------------------
fileRequest :: FilePath -> RequestFunction
fileRequest path _ _ = Right <$> B.readFile path

--------------------------------------------------------------------------------
fakeNetworkError :: RequestFunction
fakeNetworkError _ _ = return $ Left $ ServiceError "fake outage"

--------------------------------------------------------------------------------
mkDay :: Integer -> Int -> Int -> Maybe Day
mkDay y m d = Just (fromGregorian y m d)
