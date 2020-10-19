module Test.MySolutions where

import Data.Array
import Data.Char
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.String.CodeUnits
import Data.Hashable
import Prelude

newtype Complex = Complex { -- when to use newtype vs data ?
    real        :: Number, 
    imaginary   :: Number 
    }

instance showComplex :: Show Complex where
    show :: Complex -> String -- nice that this works
    -- too bad that this style doesn't work
    -- show c = show c.real <> "+" <> show c.imaginary <> "i"
    show (Complex {real, imaginary}) 
        = show real <> showSign <> show imaginary <> "i" where
            showSign = if imaginary < 0.0 then "" else "+"

instance eqComplex :: Eq Complex where
    eq :: Complex -> Complex -> Boolean
    eq 
        (Complex {real: r1, imaginary: i1}) 
        (Complex {real: r2, imaginary: i2})
        = r1 == r2 && i1 == i2

data NonEmpty a = NonEmpty a (Array a)

{- why is this needed ??? -}
instance showNonEmpty :: Show a => Show (NonEmpty a) where
    show (NonEmpty x rest) = show x <> show rest

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
    eq :: forall a. Eq a => NonEmpty a -> NonEmpty a -> Boolean
    eq 
        (NonEmpty x1 rest1)
        (NonEmpty x2 rest2)
        = x1 == x2 && rest1 == rest2

instance semigroupNonEmpty ::  Semigroup (NonEmpty a) where
    append :: forall a. NonEmpty a -> NonEmpty a -> NonEmpty a
    append 
        (NonEmpty x1 rest1)
        (NonEmpty x2 rest2)
        = NonEmpty x1 (rest1 <> [x2] <> rest2)

instance functorNonEmpty :: Functor NonEmpty where
    map :: forall a b. ( a->b ) -> NonEmpty a -> NonEmpty b
    map f (NonEmpty x rest) = NonEmpty (f x) (map f rest)        

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
    eq :: forall a. Eq a => Extended a -> Extended a -> Boolean
    eq Infinite    Infinite  = true
    eq (Finite x) (Finite y) = x == y
    eq  _          _         = false

instance ordExtended :: Ord a => Ord (Extended a) where    
    compare :: forall a. Ord a => Extended a -> Extended a -> Ordering
    compare (Finite x) (Finite y) = compare x y
    compare Infinite    Infinite  = EQ
    compare _           Infinite  = LT
    compare Infinite    _         = GT

-- follow the types
instance foldableNonEmpty :: Foldable NonEmpty where
    foldr :: forall a b. (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f start (NonEmpty x rest) = foldr f start ([x] <> rest)

    foldl :: forall a b. (b -> a -> b) -> b -> NonEmpty a -> b
    foldl f start (NonEmpty x rest) = foldl f start ([x] <> rest)

    foldMap :: forall a m. Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (NonEmpty x rest) = foldMap f ([x] <> rest)

-- f is supposed to be foldable: a container with elements in sequence
data OneMore f a = OneMore a (f a) 

-- follow the types and then follow the test :-)
instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
    foldr :: forall a b foldable. Foldable foldable => 
        (a -> b -> b)       -> 
        b                   -> 
        OneMore foldable a  -> 
        b
    foldr f start (OneMore x foldable) = f x (foldr f start foldable)

    foldl :: forall a b foldable. Foldable foldable => 
        (b -> a -> b)       -> 
        b                   -> 
        OneMore foldable a  -> 
        b
    foldl f start (OneMore x foldable) = foldl f (f start x) foldable

    foldMap :: forall a m foldable. Monoid m => Foldable foldable => 
        (a -> m)            -> 
        OneMore foldable a  -> 
        m
    foldMap f (OneMore x foldable) = f x <> foldMap f foldable

-- multiparameter type classes and functional dependencies
-- are advanced concepts (but really needed in practical code)

-- Can nullary type classes be used to track side effects in the
-- type system - an alternative to IO, Eff, Effect ? But then, "unsafe"...

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum = fromJust <<< maximum            -- ugly

class Monoid m <= Action m a where
    act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
    append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
    mempty = Multiply 1

instance multiplyIntAction :: Action Multiply Int where
    act (Multiply x) i = x * i

instance multiplyStringAction :: Action Multiply String where
    act (Multiply x) str = power str x  -- why not "repeat" (cmp. "append")

instance multiplyArrayIntAction :: Action Multiply (Array Int) where
    act (Multiply x) intArray = map (_ * x) intArray 
    
instance multiplyArrayStringAction :: Action Multiply (Array String) where
    act (Multiply x) strArray = map (\str -> power str x) strArray    

newtype Self m = Self m      -- why is this a newtype and not data ?

instance semigroupSelf :: Semigroup m => Semigroup (Self m) where
    append (Self n) (Self m) = Self (n <> m)

instance monoidSelf :: Monoid m => Monoid (Self m) where
    mempty = Self mempty

instance eqSelf :: Eq m => Eq (Self m) where
    eq (Self m) (Self n) = n == m    

instance eqMultiply :: Eq Multiply where
    eq (Multiply m) (Multiply n) = n == m

instance showSelf :: Show m => Show (Self m) where
    show (Self m) = "Self " <> show m

instance showMultiply :: Show Multiply where
    show (Multiply i) = "Multiply " <> show i

instance selfAction :: Monoid m => Action m (Self m) where
    act :: forall m. Monoid m => m -> Self m -> Self m
    act m (Self n) = Self (m <> n)            -- the sequence doesn't matter :-)

-- this was surprising since this use case is not mentioned in the exercise text
-- happily, we have the compiler messages
instance selfMultiplyAction :: Action (Self Multiply) Int where 
    act :: Self Multiply -> Int -> Int
    act (Self (Multiply x)) i = i * x    

--- (5) There is no functional dependency between the
--- parameters of the Action type class. One counter example is 
--- Action Multiply (Array Int)
--- Action Multiply (Array String)    


arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates array = nubByEq myCompare array /= array where
    myCompare x y = if hash x == hash y
        then x == y
        else false

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
    eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
    hash (Hour h) = hash (h `mod` 12)  -- only equal Hours must produce the same hash
