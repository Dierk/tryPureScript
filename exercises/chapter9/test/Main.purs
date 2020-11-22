module Test.Main where

import Prelude
import Test.MySolutions

import Data.Array (foldM, reverse)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, message, runAff_)
import Effect.Class.Console (log, logShow)


testCountCharacters :: Aff Unit
testCountCharacters = do
  result <- countCharacters "test/file1.txt" 
  case result of
    Left e      -> log $ "There was a problem with countCharacters: " <> message e
    Right count -> log $ "character count in file1: " <> show count

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


testConcatenateMany :: Aff Unit
testConcatenateMany = do
  result <- attempt $ concatenateMany ["test/file1.txt", "test/file2.txt", "test/file3.txt"] "test/file4.txt"
  case result of
    Left e -> log $ "There was a problem with concatenateMany: " <> message e
    _ -> pure unit 

testConcatenateManyParallel :: Aff Unit
testConcatenateManyParallel = do
  result <- attempt $ concatenateManyParallel ["test/file1.txt", "test/file2.txt", "test/file3.txt"] "test/file5.txt"
  case result of
    Left e -> log $ "There was a problem with concatenateManyParallel: " <> message e
    _ -> pure unit    

testGetWithTimeout :: Aff Unit
testGetWithTimeout = do
  result <- getWithTimeout 10.0 "http://www.fhnw.ch"
  case result of 
    Nothing -> log "could not fetch url in time"
    Just c  -> log c       

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
  runInSequence [
    log "async start", 
    testCountCharacters,
    testCopy, 
    testConcatenate, 
    testConcatenateMany,
    testConcatenateManyParallel,
    testGetWithTimeout,
    log "async end"
    ]
  flip runAff_ (recurseFiles ["test/root.txt"]) (either (log <<< message) (\result -> logShow result)) 
  log "sync end" -- note that this may be seen before "async end"
  -- was:   
  -- runAffTest testCopy $
    -- runAffTest testConcatenate $
      -- log "done"

