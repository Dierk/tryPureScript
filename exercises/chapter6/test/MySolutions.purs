module Test.MySolutions where

import Data.Maybe
import Prelude

import Data.Array ((:))
import Data.Foldable (class Foldable, maximum, foldr, foldl, foldMap)
import Data.Monoid (power)
import Data.Hashable

-- Note to reader: Add your solutions to this file
newtype Complex = Complex
    { real      :: Number
    , imaginary :: Number
    }

instance showComplex :: Show Complex where
  show :: Complex -> String
  show (Complex { real, imaginary }) =
    let
      op = if imaginary < 0.0 then "" else "+"
    in
      show real <> op <> show imaginary <> "i"

instance eqComplex :: Eq Complex where
  eq :: Complex -> Complex -> Boolean
  eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
  eq :: NonEmpty a -> NonEmpty a -> Boolean
  eq (NonEmpty a as) (NonEmpty b bs) = a == b && as == bs

instance showNonEmpty :: (Show a, Show (Array a)) => Show (NonEmpty a) where
  show :: NonEmpty a -> String
  show (NonEmpty a as) = show a <> " " <> show as

instance semiGroupNonEmpty :: Semigroup (Array a) => Semigroup (NonEmpty a) where
  append :: NonEmpty a -> NonEmpty a -> NonEmpty a
  append (NonEmpty a as) (NonEmpty b bs) = NonEmpty a (as <> b : bs)

instance functorNonEmpty :: Functor Array => Functor NonEmpty where
  map :: forall a b . (a -> b) -> NonEmpty a -> NonEmpty b
  map f (NonEmpty a as) = NonEmpty (f a) (f <$> as)

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq :: Extended a -> Extended a -> Boolean
  eq (Finite a) (Finite b) = a == b
  eq Infinite Infinite     = true
  eq _ _                   = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare :: Extended a -> Extended a -> Ordering
  compare (Finite a) (Finite b) = compare a b
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT

instance foldNonEmpty :: Foldable Array => Foldable NonEmpty where
  foldr :: forall a b . (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f acc (NonEmpty a as) = foldr f acc $ a : as

  foldl :: forall a b . (b -> a -> b) -> b -> NonEmpty a -> b
  foldl f acc (NonEmpty a as) = foldl f acc $ a : as

  foldMap :: forall a m . Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (NonEmpty a as) = foldMap f (a : as)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr :: forall a b . (a -> b -> b) -> b -> OneMore f a -> b
  foldr f acc (OneMore front tail) = f front newElem where
    newElem = foldr f acc tail

  foldl :: forall a b . (b -> a -> b) -> b -> OneMore f a -> b
  foldl f acc (OneMore front tail) = foldl f newElem tail where
    newElem = f acc front

  foldMap :: forall a m . Monoid m => (a -> m) -> OneMore f a -> m
  foldMap f (OneMore front tail) = f front <> (foldMap f tail)

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum = fromJust <<< maximum

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiply :: Action Multiply Int where
  act (Multiply n) m = n * m

instance showMultiply :: Show Multiply where
  show (Multiply n) = "Multiply " <> show n

instance eqMultiply :: Eq Multiply where
  eq :: Multiply -> Multiply -> Boolean
  eq (Multiply m) (Multiply n) = n == m

instance repeatAction :: Action Multiply String where
  act :: Multiply -> String -> String
  act (Multiply n) s = power s n

instance actionArray :: Action m a => Action m (Array a) where
  act :: m -> Array a -> Array a
  act m xs = act m <$> xs

newtype Self m = Self m

instance actionSelf :: Monoid m => Action m (Self m) where
  act :: m -> Self m -> Self m
  act m (Self s) = Self $ m <> s

instance eqSelf :: Eq m => Eq (Self m) where
  eq :: Self m -> Self m -> Boolean
  eq (Self a) (Self b) = a == b

instance monoidSelf :: Monoid m => Monoid (Self m) where
  mempty :: forall m . Monoid m => Self m
  mempty = Self mempty

instance semigroupSelf :: Semigroup m => Semigroup (Self m) where
  append :: Semigroup m => Self m -> Self m -> Self m
  append (Self a) (Self b) = Self $ a <> b

instance actionMultiplySelf :: Action (Self Multiply) Int where
  act :: Self Multiply -> Int -> Int
  act (Self (Multiply a)) n = n * a

instance showSelf :: Show m => Show (Self m) where
  show (Self m) = "Self " <> show m

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
  hash (Hour h) = hash (h `mod` 12)
