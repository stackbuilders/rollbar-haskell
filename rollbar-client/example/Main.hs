module Main
  ( main
  ) where

import qualified Data.Text as Text
import Rollbar.Client
import System.Environment (getArgs)

-- To load this example using Cabal, run:
--
-- ROLLBAR_TOKEN=<...> cabal repl --flags example client-example
--
-- Or, if using Stack, replace the Cabal part with:
--
-- stack ghci --flag rollbar-client:example rollbar-client:exe:client-example
--
-- You can then test creating a Rollbar item. If there are no arguments, the
-- example uses the 'withRollbar' function, which automatically creates a
-- Rollbar item after catching an exception:
--
-- >>> :main
-- *** Exception: Boom
-- CallStack (from HasCallStack):
--   error, called at .../rollbar-haskell/rollbar-client/example/Main.hs:39:30 in main:Main
--
-- If there are arguments, the example uses the 'runRollbar' function, which
-- allows us to create and send our own Rollbar items. In this case, we send
-- the arguments as message and change the level to warning:
--
-- >>> :main Warning!
-- ItemId "55c3e83084d943d6af3bf940e80513e1"

main :: IO ()
main = do
  settings <- readSettings "rollbar.yaml"
  args <- getArgs
  case args of
    [] ->
      withRollbar settings $ error "Boom"
    _ -> do
      itemId <-
        runRollbar settings $ do
          let body = Text.pack (unwords args)
          item <- mkItem (PayloadMessage (Message body mempty))
          createItem item { itemLevel = Just LevelWarning }
      print itemId
