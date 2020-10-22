module Test.MySolutions where

import Control.Apply
import Data.AddressBook
import Data.AddressBook.Validation
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Data.Validation.Semigroup
import Prelude

import Data.Either (either, fromRight)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable 
import Partial.Unsafe (unsafePartial)

-- lift3 is well motivated. Note that 'map' also lifts.
-- <> $ <$> <*>
-- "apply" makes cool use of curried style plus right associativity of -> function definition
-- which makes the <*> operation monoidal.

-- "new, larger language" is a bit over the top. I actually find it more a sublanguage, an internal DSL.
-- ado .. in
-- Applicative "effect" or "side-effect" is not well motivated. sequence, caching, ...?

-- addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
-- addMaybe (Just x) (Just y) = Just (x+y)  -- nicely shows that this is a lifting of (+)
-- addMaybe  _        _       = Nothing 

addMaybe :: forall m. Apply m => m Int -> m Int -> m Int
addMaybe = lift2 (+)
subMaybe = lift2 (-)
mulMaybe = lift2 (*)
divMaybe = lift2 (/)

addApply = lift2 (+)
subApply = lift2 (-)
mulApply = lift2 (*)
divApply = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe  Nothing  = pure Nothing
combineMaybe (Just fa) = Just <$> fa
-- combineMaybe (Just fa) = pure Just <*> fa        -- works as well

-- the validation use case is cool but at the same time
-- seems a bit overly complicated for such a simple task.
-- Where is the striking benefit? Compositionality of validations? Modularisation?

-- note pn.type can be written as pn."type" to avoid the reserved keyword restriction


failingRegex :: Regex
failingRegex = unsafePartial $ fromRight $ regex "(?!)" noFlags 

--- only for literal Strings
staticRegex :: String -> Regex
-- staticRegex str = unsafePartial $ fromRight $ regex str noFlags                    -- neither is nice
staticRegex str = either (const failingRegex) identity $ regex str noFlags

stateRegex :: Regex
stateRegex = staticRegex "^[a-zA-Z]{2,2}$"         -- no special escaping syntax?

nonEmptyRegex :: Regex
nonEmptyRegex = staticRegex "\\S"                  -- in other words: at least one non-space char


validateState :: String -> V Errors String
validateState state = matches "State" stateRegex state *> pure state

validateNonEmpty :: String -> String -> V Errors String
validateNonEmpty fieldName value = matches fieldName nonEmptyRegex value *> pure value

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> (validateNonEmpty "Street" a.street)
          <*> (validateNonEmpty "City"   a.city  )
          <*> (validateState a.state)

-- why "applicatives track side-effects" ? They rather ensure a sequence of operations...

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: Show a => Show (Tree a) where
    show :: forall a. Show a => Tree a -> String
    show Leaf = 
        "Leaf"
    show (Branch left value right) = 
        "(Branch " <> show left <> " " <> show value <> " " <> show right <> ")"    

instance eqTree :: Eq a => Eq (Tree a) where
    eq :: forall a. Eq a => Tree a -> Tree a -> Boolean
    eq (Branch l1 v1 r1) (Branch l2 v2 r2) =
        v1 == v2 && l1 == l2 && r1 == r2 
    eq Leaf Leaf = 
        true        
    eq _ _ =
        false    

instance functorTree :: Functor Tree where
    map :: forall a b. (a->b) -> Tree a -> Tree b
    map f Leaf = Leaf
    map f (Branch l v r) = Branch (f <$> l) (f v) (f <$> r)

instance foldableTree :: Foldable Tree where
    foldl :: forall a b . (b -> a -> b) -> b -> Tree a -> b
    foldl f start Leaf = start
    foldl f start (Branch l v r) =  leftAndValueAndRight  where
        left                 = foldl f start l
        leftAndValue         = f left v
        leftAndValueAndRight = foldl f leftAndValue r

    foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
    foldr f start Leaf = start
    foldr f start (Branch l v r) = leftAndValueAndRight where 
        right                = foldr f start r
        valueAndRight        = f v right 
        leftAndValueAndRight = foldr f valueAndRight l

    foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
    foldMap f Leaf = mempty
    foldMap f tree =  foldl (<>) mempty monoidalTree where -- monoids are associative so both foldr and foldr work
        monoidalTree = f <$> tree   

instance traversableTree :: Traversable Tree where
    traverse :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)    
    traverse f tree = sequence applicativeTree where
        applicativeTree = f <$> tree                    -- Tree (m b)

    sequence :: forall a m. Applicative m => Tree (m a) -> m (Tree a)
    sequence Leaf = pure Leaf
    sequence (Branch l v r) = Branch <$> (sequence l) <*> v <*> (sequence r)   -- mapping of partial functions ;-)

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder a2mb Leaf = pure Leaf
traversePreOrder a2mb (Branch l v r) = ado
    value <- a2mb v                     -- sequence matters
    left  <- traversePreOrder a2mb l
    right <- traversePreOrder a2mb r 
    in Branch left value right

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder a2mb Leaf = pure Leaf
traversePostOrder a2mb (Branch l v r) = ado
    left  <- traversePostOrder a2mb l
    right <- traversePostOrder a2mb r 
    value <- a2mb v                     -- sequence matters
    in Branch left value right

type PersonOptionalAddress
  = { firstName     :: String
    , lastName      :: String
    , homeAddress   :: Maybe Address
    , phones        :: Array PhoneNumber
    }

personOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p =
  personOptionalAddress <$> (nonEmpty "First Name" p.firstName *> pure p.firstName)
         <*> (nonEmpty "Last Name"  p.lastName  *> pure p.lastName)
         <*> (traverse validateAddress p.homeAddress )  -- in validation below the maybe, then put the result over 
         <*> (arrayNonEmpty "Phone Numbers" p.phones *>
              traverse validatePhoneNumber p.phones)

-- sequenceUsingTraverse :: Array (Maybe Int) -> Maybe (Array Int)
sequenceUsingTraverse :: forall t m a. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse = traverse identity     

-- traverseUsingSequence ::  (Number -> Maybe Int) -> Array Number -> Maybe (Array Int)
traverseUsingSequence :: forall a b t m. Applicative m => Traversable t =>  (a -> m b) -> t a -> m (t b)
traverseUsingSequence f intArray = sequence $ f <$> intArray

-- traverse is somewhat similar to foldMap: first map, then do your other work

