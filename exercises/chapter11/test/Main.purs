module Test.Main where

import Prelude
import Test.MySolutions
import Control.Monad.State
import Data.Tuple

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
 runTest do
    suite "a suite of test suites" do
      suite "first suite" do
        test "equality test 1"
          $ Assert.equal 1
          $ 1
        test "equality test 2"
          $ Assert.equal 2
          $ 2
      suite "second suite" do
        test "equality test 1"
          $ Assert.equal 1
          $ 1
        test "equality test 2"
          $ Assert.equal 2
          $ 2
      suite "State functions" do
        test "execState returns the inner state"
          $ Assert.equal 21
          $ stateExample execState
        test "evalState evaluates the running value with regard to the State, here: unit"
          $ Assert.equal unit
          $ stateExample evalState
        test "runState returns a tuple of running value and inner state"
          $ Assert.equal (Tuple unit 21)
          $ stateExample runState

{-
    What is the difference between ST and State monad?
    ST provides a place for STRefs to live in.
    State updates a data structure "on the side" while working with a running value.

    see Brian Beckman: The Zen of Stateless State - The State Monad
    https://www.youtube.com/watch?v=XxzzJiXHOJs

    Balanced parentheses, see /experimental

-}
