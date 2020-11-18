module Test.MySolutions where

import Data.Maybe
import Data.Traversable
import Prelude

import Control.Apply (lift2)

-- Note to reader: Add your solutions to this file

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe x y = lift2 (+) x y

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe x y = lift2 (-) x y

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe x y = lift2 (*) x y

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe x y = lift2 (/) x y

addApply :: forall f. Apply f => f Int -> f Int -> f Int
addApply x y = lift2 (+) x y

subApply :: forall f. Apply f => f Int -> f Int -> f Int
subApply x y = lift2 (-) x y

mulApply :: forall f. Apply f => f Int -> f Int -> f Int
mulApply x y = lift2 (*) x y

divApply :: forall f. Apply f => f Int -> f Int -> f Int
divApply x y = lift2 (/) x y

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just f) = map Just f

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance eqTree :: Eq a => Eq (Tree a) where
  eq :: Tree a -> Tree a -> Boolean
  eq Leaf Leaf = true
  eq (Branch l1 m1 r1) (Branch l2 m2 r2) = l1 == l2 && m1 == m2 && r1 == r2
  eq _ _ = false

instance showTree :: Show a => Show (Tree a) where
  show :: Tree a -> String
  show Leaf = "Leaf"
  show (Branch l m r) = "(Branch " <> show l <> " " <> show m <> " " <> show r <> ")"

instance functorTree :: Functor Tree where
  map :: forall a b. (a -> b) -> Tree a -> Tree b
  map f Leaf = Leaf
  map f (Branch l m r) = Branch (f <$> l) (f m) (f <$> r)

instance foldableTree :: Foldable Tree where
  foldr :: forall a b . (a -> b -> b) -> b -> Tree a -> b
  foldr f acc Leaf = acc
  foldr f acc (Branch l m r) = res where
    right = foldr f acc r
    middleAndRight = f m right
    res = foldr f middleAndRight l

  foldl :: forall a b . (b -> a -> b) -> b -> Tree a -> b
  foldl f acc Leaf = acc
  foldl f acc (Branch l m r) = res where
    left = foldl f acc l
    middleAndLeft = f left m
    res = foldl f middleAndLeft r

  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f (Branch l m r) = foldMap f l <> f m <> foldMap f r

instance traversableTree :: Traversable Tree where
  traverse :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch l m r) = ado
   left <- traverse f l
   middle <- f m
   right <- traverse f r
   in Branch left middle right

  sequence :: forall a m. Applicative m => Tree (m a) -> m (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch l m r) = Branch <$> sequence l <*> m <*> sequence r

traversePreOrder :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch l m r) = ado
  middle <- f m
  left <- traversePreOrder f l
  right <- traversePreOrder f r
  in Branch left middle right

traversePostOrder :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch l m r) = ado
  left <- traversePostOrder f l
  right <- traversePostOrder f r
  middle <- f m
  in Branch left middle right

sequenceUsingTraverse :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse t = traverse identity t

traverseUsingSequence :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f t = sequence $ map f t