module Test.MySolutions where

import Prelude
import Data.Array (head, tail)
import Data.Maybe
import Data.List

third :: forall a. Array a -> Maybe a
third xs = do
  fst <- tail xs
  snd <- tail fst
  head snd

{- 3. ap for Maybe
ap :: forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
ap Nothing _ = Nothing
ap (Just f) a = do
  f' <- f
  a' <- a
  pure (f' a')

apply :: forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
apply (Just f) (Just a) = Just (f a)
apply _ _               = Nothing
-}
{- 4. Monad laws for Maybe monad
instance bindMaybe :: Bind Maybe where
  bind :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
  bind Nothing  _ = Nothing
  bind (Just a) f = f a
-}
filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil

filterM f (x : xs) = do
  pred <- f x
  xs'  <- filterM f xs
  pure if pred then x : xs' else xs'