module Main
  ( main
  ) where

import Control.Monad (unless)
import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure)

main :: IO ()
main = do
  hints <- hlint ["example", "src", "test"]
  unless (null hints) exitFailure
