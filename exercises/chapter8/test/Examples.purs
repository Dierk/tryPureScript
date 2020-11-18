module Test.Examples where

import Prelude

import Control.Plus (empty)
import Data.Array ((..), foldM, sort, nub)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect.Exception (throwException)
import Effect (Effect)
import Effect.Aff (error)

{-| Monads and Do Notation -}
countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n then
    pure [ x, y ]
  else
    empty

{-| Folding With Monads -}
foldM' ::
  forall m a b.
  Monad m =>
  (a -> b -> m a) ->
  a ->
  List b ->
  m a
foldM' _ a Nil = pure a

foldM' f a (b : bs) = do
  a' <- f a b
  foldM' f a' bs

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing

safeDivide a b = Just (a / b)

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "Divide by 0"
exceptionDivide a b = pure $ a / b

possibleSums :: Array Int -> Array Int
possibleSums xs = sort $ nub $ foldM (\x -> \y -> [x, x + y]) 0 xs