module Test.MySolutions where

import Data.Array (nubByEq, (:))
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Prelude (class Eq, class Functor, class Monoid, class Ord, class Semigroup, class Show, 
  Ordering(..), append, compare, map, mempty, mod, not, otherwise, show, ($), (&&), (*), (<>), (==), (>))
import Data.Hashable

-- Note to reader: Add your solutions to this file

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex {real: r, imaginary: i}) | i > 0.0 = show r <> "+" <> show i <> "i"
                                         | otherwise = show r <> show i <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary 

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty a xs) = show a <> " " <> show xs
-- darstellung fraglich  

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a xs) (NonEmpty a1 ys) = a == a1 && xs == ys 

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a xs) (NonEmpty a1 ys) = (NonEmpty a (xs <> (a1 : ys)))

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a xs) = NonEmpty (f a) (map f xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f b (NonEmpty e xs) = foldr f b ([ e ] <> xs)
  foldl f b (NonEmpty e xs) = foldl f b ([ e ] <> xs)
  foldMap f (NonEmpty e xs) = foldMap f ([ e ] <> xs)

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite n) (Finite n1) = n == n1
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite n) (Finite n1) = compare n n1


data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f b (OneMore val more) = f val (foldr f b more)
  foldl f b (OneMore val more) = foldl f (f b val) more
  foldMap f (OneMore val more) = f val <> foldMap f more


-- etwas unsinnig
unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum arr = case maximum arr of
  Just a -> a

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiply :: Action Multiply Int where
  act (Multiply m) n = m * n

instance eqMultiply :: Eq Multiply where
  eq (Multiply m) (Multiply n) = m == n

instance repeatAction :: Action Multiply String where
  act (Multiply m) n = power n m

instance actionArray :: Action m a => Action m (Array a) where
  act m n = map (act m) n

instance showMultiply :: Show Multiply where
  show (Multiply m) = "Multiply " <> show m


newtype Self m = Self m

instance actionSelf :: Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

instance eqSelf :: Eq m => Eq (Self m) where
  eq (Self m) (Self n) = m == n

instance semigroupSelf :: Semigroup s => Semigroup (Self s) where
  append (Self m) (Self n) = Self (append m n)

instance monoidSelf :: Monoid m => Monoid (Self m) where
  mempty = Self mempty

instance repeatActionMultSelf :: Action (Self Multiply) Int where
  act (Self (Multiply m)) n = m * n

instance showSelf :: Show m => Show (Self m) where
  show (Self m) = "Self " <> show m


arrayHasDuplicates :: forall a. Hashable a => (Array a) -> Boolean
arrayHasDuplicates arr = not (hash arr == hash (nubByEq (==) arr))
-- falsch

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates array = nubByEq myCompare array /= array where
  myCompare x y = if hash x == hash y
                  then x == y
                  else false


newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
  hash (Hour i) = hash $ mod i 12
