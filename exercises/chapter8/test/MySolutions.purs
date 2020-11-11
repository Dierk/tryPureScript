module Test.MySolutions where

import Data.List
import Prelude

import Data.Array as Arr
import Data.Int.Bits (xor)
import Data.Maybe (Maybe(..))
import Effect.Exception
import Effect

-- Note to reader: Add your solutions to this file

third :: forall a. Array a -> Maybe a
third arr = do
  t1 <- Arr.tail arr
  t2 <- Arr.tail t1
  Arr.head t2

possibleSums :: Array Int -> Array Int
possibleSums arr = Arr.sort $ Arr.foldM (\acc y -> [acc, acc + y]) 0 (Arr.nub arr)

{-
ap :: forall a b. Monad Maybe => Maybe (a -> b) -> Maybe a -> Maybe b
ap mf ma = do
  f <- mf
  a <- ma
  Just (f a)

instance applyMaybe :: Apply Maybe where
  apply :: forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
  apply (Just f) (Just x) = Just (f x)
  apply _        _        = Nothing
-}

{-
class (Applicative m, Bind m) <= Monad m

class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

instance bindMaybe :: Bind Maybe where
  bind Nothing  _ = Nothing
  bind (Just a) f = f a

class Apply f <= Applicative f where
  pure :: forall a. a -> f a

instance applicativeMaybe :: Applicative Maybe where
  pure x = Just x

1. right-identity ok
2. left-identity ok
3. associativity law ok
-}

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM f Nil = pure Nil
filterM f (x:xs) = do
  b <- f x
  r <- filterM f xs
  pure if b then x : r else r

{-

lift2 :: forall f a b c. Apply Maybe => (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift2 f (Just a) (Just b) = Just (f a b)  => ok

lift2 :: forall f a b c. Apply f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b

ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
  f <- mf
  a <- ma
  pure (f a)

class (Applicative m, Bind m) <= Monad m

class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class Apply f <= Applicative f where
  pure :: forall a. a -> f a
-}

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

exceptionDivide :: Number -> Number -> Effect Number
exceptionDivide _ 0.0 = throwException $ error "division by 0"
exceptionDivide a b = pure (a / b)


