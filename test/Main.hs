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
module Main (main) where

import qualified MovieTest as M
import qualified TVTest as T
import Test.Tasty

tests :: TestTree
tests = testGroup "Tests" [M.tests, T.tests]

main :: IO ()
main = defaultMain tests
