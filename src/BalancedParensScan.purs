
module BalancedParensScan where

import Prelude 
import Effect (Effect)
import Effect.Console (logShow)
import Data.Foldable (traverse_)
import Control.Monad.State (State, execState) 
import Control.Monad.State.Class (modify)

import Data.String.CodeUnits (toCharArray)

-- standard way of using traverse (only for reference)

sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

trySumArray :: Int
trySumArray = execState (sumArray [1,2,3]) 0

-- Exercise from Purescript by Example chapter 11: test for balanced parentheses
-- a) use traverse and State Monad to check for balanced parens

type Scan = { pb  :: Int      -- parentheses balance
            , pos :: Boolean  -- was always stay positive
            }

balArray :: Array Char -> State Scan Unit
balArray = traverse_ \c -> modify \scan -> scan {
  pb  = scan.pb + charValue c,
  pos = scan.pb >= 0 && scan.pos
  }

isBalanced :: String -> Boolean
isBalanced cs = isValid scan where
  isValid {pb: 0, pos: true} = true
  isValid _ = false
  scan      = execState (balArray (toCharArray cs)) {pb: 0, pos: true}

charValue :: Char -> Int
charValue '(' =  1
charValue ')' = -1
charValue _   =  0

main :: Effect Unit
main = do
  logShow $ trySumArray
  logShow $ false == isBalanced  "("
  logShow $ false == isBalanced  ")"
  logShow $ true  == isBalanced  "()"
  logShow $ false == isBalanced  ")()"
  logShow $ false == isBalanced  "(()"
  logShow $ false == isBalanced  "())"
  logShow $ true  == isBalanced  "((()())())()"
