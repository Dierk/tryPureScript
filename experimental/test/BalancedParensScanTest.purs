

module Test.BalancedParensScanTest where

import Prelude
import Effect (Effect)
import Test.Assert (assert)
import BalancedParensScan (isBalanced)

main :: Effect Unit
main = do
  assert $ false == isBalanced  "("
  assert $ false == isBalanced  ")"
  assert $ true  == isBalanced  "()"
  assert $ false == isBalanced  ")()"
  assert $ false == isBalanced  "(()"
  assert $ false == isBalanced  "())"
  assert $ true  == isBalanced  "((()())())()"
  
