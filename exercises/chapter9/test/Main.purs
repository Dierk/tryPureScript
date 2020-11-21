module Test.Main where

import Prelude
import Test.MySolutions

import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Aff (Aff, attempt, message, runAff_)
import Effect.Class.Console (log)
import Web.File.FileReader (result)


testCopy :: Aff Unit
testCopy = do
  result <- attempt $ copyFile "test/file1.txt" "test/file2.txt"
  case result of
    Left e -> log $ "There was a problem with copyFile: " <> message e
    _ -> pure unit

testConcatenate :: Aff Unit
testConcatenate = do
  result <- attempt $ concatenateFiles "test/file1.txt" "test/file2.txt" "test/file3.txt"
  case result of
    Left e -> log $ "There was a problem with concatenateFiles: " <> message e
    _ -> pure unit

runAffTest :: forall a. Aff a -> Effect Unit -> Effect Unit
runAffTest affTest continuation = flip runAff_ affTest $ either 
  (log <<< message) 
  (\_ -> continuation)

foldM :: forall a. (Effect Unit -> Aff a -> Effect Unit) -> Unit -> Array (Aff a) -> Effect (Effect Unit)

runInSequence affTests = do 
  result <- foldM ( \effect affTest -> runAffTest affTest effect) unit affTests
  bare   <- result
  pure bare

main :: Effect Unit
main = do
  runInSequence [testCopy, (log "done")]
  --runAffTest testCopy $
    --runAffTest testConcatenate $
      --log "done"

