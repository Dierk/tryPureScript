module Test.Main where

import Prelude
import Test.MySolutions

import Data.Either (Either(..),either)
import Effect (Effect)
import Effect.Aff (Aff, attempt, message, runAff_)
import Effect.Class.Console (log)


testCopy :: Aff Unit
testCopy = do
  result <- attempt $ copyFile "file1.txt" "file2.txt"
  case result of
    Left e -> log $ "There was a problem with copyFile: " <> message e
    _ -> pure unit

main :: Effect Unit
main = do
  flip runAff_ testCopy $ either (log <<< message) (\_ -> log "done")
