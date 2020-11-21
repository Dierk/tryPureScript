module Test.Main where

import Prelude
import Test.MySolutions

import Data.Either (Either(..), either)
import Data.Array (foldM, reverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, message, runAff_)
import Effect.Class.Console (log)


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

-- foldM :: forall a. 
--  (Effect Unit -> Aff a -> Effect (Effect Unit)) -> 
--  (Effect Unit) -> 
--  Array (Aff a) -> 
--  Effect (Effect Unit)

--- Let the async effects run async but in sequence
runInSequence :: forall a. Array (Aff a) -> Effect Unit
runInSequence affTests = 
  join $ foldM (\effect affTest -> pure $ runAffTest affTest effect ) 
            (pure unit) 
            (reverse affTests)

main :: Effect Unit
main = do
  -- note that log works for _all_ Monadic Effects, i.e. also "Aff"
  runInSequence [log "start", testCopy, testConcatenate, log "done"]
  log "end" -- note that this may be seen before "done"
  -- was:   
  -- runAffTest testCopy $
    -- runAffTest testConcatenate $
      -- log "done"

