module Test.Main where

import Data.Traversable
import Prelude
import Random.LCG

import Data.Array (sort, sortBy, snoc)
import Data.Foldable (foldr)
import Data.Function (on)
import Data.List (List(..), fromFoldable)
import Data.NonEmpty (NonEmpty(..), (:|))
import Effect (Effect)
import Merge (mergeWith, mergePoly, merge)
import Sorted (sorted)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary, quickCheck, quickCheckPure, (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (elements, oneOf, Gen(..))
import Tree (Tree(..), member, insert, toArray, anywhere)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

ints :: Array Int -> Array Int
ints = identity

s2s :: S2 -> S2
s2s = identity 

bools :: Array Boolean -> Array Boolean
bools = identity

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = identity

treeOfInt :: Tree Int -> Tree Int
treeOfInt = identity

newtype S2 = S2 String 

instance arbS2 :: Arbitrary S2 where
  arbitrary = elements (NonEmpty (S2 "a") [S2 "b", S2 "c", S2 "z"])


instance eqS2 :: Eq S2 where
  eq (S2 a) (S2 b) = a == b

instance showS2 :: Show S2 where
  show (S2 s) = show s
  

data OneTwoThree a = One a | Two a a | Three a a a

-- data NonEmpty f a
-- NonEmpty a (f a)
--oneOf :: forall a. NonEmpty Array (Gen a) -> Gen a
-- elements :: forall a. NonEmpty Array a -> Gen a

instance arbOneTwoThree ::(Arbitrary a) => Arbitrary (OneTwoThree a) where
  arbitrary = oneOf ((One <$> arbitrary) :| [(Two <$> arbitrary <*> arbitrary ),(Three <$> arbitrary<*> arbitrary<*> arbitrary) ])

instance coarbOneTwoThree :: (Coarbitrary a) => Coarbitrary (OneTwoThree a) where
  coarbitrary (One a) = coarbitrary a
  coarbitrary (Two a b) = coarbitrary a >>> coarbitrary b 
  coarbitrary (Three a b c) = coarbitrary a >>> coarbitrary b >>> coarbitrary c


--test :: forall a. 
--fromA :: forall a . Arbitrary a => Traversable a => a -> Gen (OneTwoThree a)
--fromA a = sequence (One arbitrary)

main :: Effect Unit
main = do
  -- Tests for module 'Merge'

 {- quickCheck \s ->
    let
      result = s2s s
      expected = S2 "a"
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected-}

  quickCheck \xs ys ->
    let
      result = merge (sort xs) (sort ys)
      expected = sort $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ->
    let
      result = merge (sort xs) []
      expected = sort xs
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ys ->
    let
      result = merge (sorted xs) (sorted ys)
      expected = sort $ sorted xs <> sorted ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ys ->
      let
      result = ints $ mergePoly (sorted xs) (sorted ys)
      expected = sort $ sorted xs <> sorted ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected

  quickCheck \xs ys ->
    let
      result = bools $ mergePoly (sorted xs) (sorted ys)
      expected = sort $ sorted xs <> sorted ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected


  quickCheck \xs ys f ->
    let
      result = map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
      expected = map f $ sortBy (compare `on` f) $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected
      
      
 {-
  quickCheckPure (mkSeed 12345) 4 \xs ys f ->
    let
      result = map f $ mergeWith f (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
      expected = map f $ sortBy (compare `on` f) $ xs <> ys
    in
      eq result expected
-}

  quickCheck \xs y ->
    let
      result = ints $ snoc xs y
      expected = xs <> [y]
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected


  -- Tests for module 'Tree'

  quickCheck \t a -> member a $ insert a $ treeOfInt t
  quickCheck \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck \f g t ->
    anywhere (\s -> f s || g s) t ==
      anywhere f (treeOfInt t) || anywhere g t


  quickCheck \arr a ->
    let
      intsarr = ints arr
      newTree = foldr insert (Branch Leaf a Leaf) intsarr
    in
      member a newTree
      
  
  {--}

  
