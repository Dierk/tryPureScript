module Test.Main where

import Prelude
import Test.MySolutions
import Control.Monad.State
import Data.Either
import Data.Tuple
import Data.Monoid.Additive

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
      suite "Reader for Doc example" do
        test "empty doc"
          $ Assert.equal ""
          $ render (line "")
        test "example doc"
          $ Assert.equal "Here is some indented text:\n I am indented\n So am I\n  I am even more indented"
          $ exampleDoc
      suite "Writer" do
        test "example for summing (kind of foolish)"
          $ Assert.equal 21
          $ writerExample
        test "collatz raw 1"
          $ Assert.equal 0
          $ collatz 1 0
        test "collatz raw 2"
          $ Assert.equal 1
          $ collatz 2 0
        test "collatz raw 3"
          $ Assert.equal 7
          $ collatz 3 0
        test "collatz with Writer 3"
          $ Assert.equal (Tuple 7 [3,10,5,16,8,4,2,1])
          $ runCollatzW 3
      suite "Monad Transformers" do
        test "safe divide 1 1"
          $ Assert.equal (Right 1)
          $ runSafeDivide 1 1
        test "safe divide 1 0"
          $ Assert.equal (Left "cannot divide by zero")
          $ runSafeDivide 1 0
        test "parser ok"
          $ Assert.equal (Right (Tuple (Tuple "abc" "def") ["The state is 'abcdef'."]))
          $ runParser (string "abc") "abcdef"
        test "parser fail"
          $ Assert.equal (Left ["No prefix 'abc' in 'bcdef'."])
          $ runParser (string "abc") "bcdef"

      suite "ReaderT and WriterT for Doc example" do
        test "example doc"
          $ Assert.equal "Here is some indented text:\n I am indented\n So am I\n  I am even more indented"
          $ renderT exampleDocT

{-
    What is the difference between ST and State monad?
    ST provides a place for STRefs to live in.
    State updates a data structure "on the side" while working with a running value.

    see Brian Beckman: The Zen of Stateless State - The State Monad
    https://www.youtube.com/watch?v=XxzzJiXHOJs

    Balanced parentheses, see /experimental

    For parsers, I would assume that one creates a specific type anyway.
-}
