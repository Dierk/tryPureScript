
module BalancedParensKmett where

import Prelude 
import Effect (Effect)
import Effect.Console (logShow)
import Data.Foldable (foldMap)

import Data.String.CodeUnits (toCharArray)

-- stateless Edward Kmett solution shown at HaskellerZ meetup (adapted):

data B = B Int Int -- number of ))), number of (((
derive instance eqB :: Eq B

instance semigroupB :: Semigroup B where
  append (B a b) (B c d)
    | b <= c = B (a + c - b) (d) -- less ( than matching )) so overflow goes to the left
    | true   = B (a) (d + b - c) -- otherwise to the right

instance monoidB :: Monoid B where
  mempty = B 0 0

parse :: Char -> B
parse ')' = B 1 0
parse '(' = B 0 1
parse _   = B 0 0

isBalanced :: String -> Boolean
isBalanced str = B 0 0 == foldMap parse (toCharArray str)

main :: Effect Unit
main = do
  logShow $ false == isBalanced  "("
  logShow $ false == isBalanced  ")"
  logShow $ true  == isBalanced  "()"
  logShow $ false == isBalanced  ")()"
  logShow $ false == isBalanced  "(()"
  logShow $ false == isBalanced  "())"
  logShow $ true  == isBalanced  "((()())())()"
