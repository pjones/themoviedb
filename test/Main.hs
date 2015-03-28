--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import qualified MovieTest as M
import qualified TVTest as T
import Test.Tasty

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [ M.tests, T.tests ]

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
