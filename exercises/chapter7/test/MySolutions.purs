module Test.MySolutions where

import Control.Apply
import Data.AddressBook
import Data.AddressBook.Validation
import Data.Foldable
import Data.Traversable
import Prelude

import Data.Array.NonEmpty (findLastIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, unV, invalid)
import Math (e)
import Partial.Unsafe (unsafePartial)

-- Note to reader: Add your solutions to this file


addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe a b = lift2 (+) a b

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe a b = lift2 (-) a b

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe a b = lift2 (*) a b

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe a b = lift2 (/) a b

addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 (-)

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 (*)

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 (/) 

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Nothing) = pure Nothing
combineMaybe (Just a) = map Just a

stateRegex :: Regex
stateRegex =
  unsafePartial case regex "^[A-Za-z]{2}$" noFlags of
    Right r -> r

nonEmptyRegex :: Regex
nonEmptyRegex =
  unsafePartial case regex "\\S" noFlags of
    Right r -> r

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved ad =
  address <$> (matches "Street" nonEmptyRegex ad.street *> pure ad.street)
    <*> (matches "City" nonEmptyRegex ad.city *> pure ad.city)
    <*> (matches "State" stateRegex ad.state *> pure ad.state)


data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: Show a => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch t1 e t2) = "(Branch " <> show t1 <> " " <> show e <> " " <> show t2 <> ")"

instance eqTree :: Eq a => Eq (Tree a) where
  eq Leaf Leaf = true
  eq Leaf _ = false
  eq _ Leaf = false
  eq (Branch t1 e1 t2) (Branch t3 e2 t4) = t1 == t3 && t2 == t4 && e1 == e2

instance functorTree :: Functor Tree where
  map :: forall a b. (a -> b) -> Tree a -> Tree b
  map _ Leaf = Leaf
  map f (Branch t1 e t2) = (Branch (map f t1) (f e) (map f t2))

instance foldableTree :: Foldable Tree where
  foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Leaf = acc
  foldr f acc (Branch t1 e t2) = foldr f (f e (foldr f acc t2)) t1

  foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
  foldl _ acc Leaf = acc
  foldl f acc (Branch t1 e t2) = foldl f (f (foldl f acc t1) e) t2

  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Branch t1 e t2) = (foldMap f t1) <> f e <> (foldMap f t2)

instance traversableTree :: Traversable Tree where
  traverse :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
  traverse f Leaf = pure Leaf
  traverse f (Branch t1 e t2) = ado
    mt1 <- traverse f t1
    mv <- f e
    mt2 <- traverse f t2
    in Branch mt1 mv mt2

  sequence :: forall a m. Applicative m => Tree (m a) -> m (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch t1 e t2) = ado
    mt1 <- sequence t1
    mv <- e
    mt2 <- sequence t2
    in Branch mt1 mv mt2


traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch t1 e t2) = ado
  mv <- f e
  mt1 <- traversePreOrder f t1
  mt2 <- traversePreOrder f t2
  in Branch mt1 mv mt2

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch t1 e t2) = ado
  mt1 <- traversePostOrder f t1
  mt2 <- traversePostOrder f t2
  mv <- f e
  in Branch mt1 mv mt2


type PersonOptionalAddress
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

personOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p = ado
  firstName <- (nonEmpty "First Name" p.firstName *> pure p.firstName)
  lastName <- (nonEmpty "Last Name" p.lastName *> pure p.lastName)
  address <- (traverse validateAddress p.homeAddress *> pure p.homeAddress)
  numbers <- (arrayNonEmpty "Phone Numbers" p.phones *> traverse validatePhoneNumber p.phones)
  in personOptionalAddress firstName lastName address numbers

-- Exercise 6
sequenceUsingTraverse :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse t = traverse identity t

-- Exercise 7
traverseUsingSequence :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f t = sequence $ map f t