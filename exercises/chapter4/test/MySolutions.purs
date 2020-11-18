module Test.MySolutions where

import Prelude

import Data.Array (null, head, tail, filter, (..), concat, last)
import Data.Array.NonEmpty (findLastIndex)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Test.Examples (factorsV3, allFiles)
import Test.QuickCheck.Gen (arrayOf)
import Control.MonadZero (guard)
import Data.Foldable(product, foldl)
import Data.Path
import Data.String.Common (split)
import Data.Tuple (Tuple(..), snd)
import Data.String.Pattern (Pattern(..))

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven x = case x of
    0 -> true
    1 -> false
    _ -> isEven $ x - 2

scoreEven :: Int -> Int
scoreEven x = if isEven x then 1 else 0

countEven :: Array Int -> Int
countEven arr =
    if null arr
        then 0
        else (scoreEven $ fromMaybe 1 $ head arr) + (countEven $ fromMaybe [] $ tail arr)

squared :: Array Number -> Array Number
squared arr = (\x -> x * x) <$> arr

keepNonNegative :: Array Number -> Array Number
keepNonNegative arr = filter (\x -> x >= 0.0) arr

infix 4 filter as <$?>
{- <$> operator has prio 4-}

isNonNegative :: Number -> Boolean
isNonNegative x = if x >= 0.0 then true else false

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = isNonNegative <$?> arr

isPrime :: Int -> Boolean
isPrime x = case x of
    1 -> false
    _ -> if pure [1, x] == factorsV3 x then true else false

primes :: Array Int -> Array Int
primes arr = isPrime <$?> arr

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs1 xs2 = do
    a <- xs1
    b <- xs2
    pure [a, b]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

factorize :: Int -> Array Int
factorize n = primes <<< concat $ factorsV3 n

allTrue :: Array Boolean -> Boolean
allTrue bools = foldl (\acc bool -> acc && bool) true bools

fibTailRec :: Int -> Int
fibTailRec n = fib' n 0 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) (n1 + n2) n1

reverse :: ∀ a. Array a -> Array a
reverse = foldl (\xs x -> [ x ] <> xs) []

onlyFiles :: Path -> Array Path
onlyFiles p = filter (\p' -> not $ isDirectory p') $ allFiles p

maxSigned32BitInt :: Int
maxSigned32BitInt = 2147483647

largestSmallest :: Path -> Array (Tuple String Int)
largestSmallest path = largestSmallestPaths (allFiles path)
  where
  largestSmallestPaths :: Array Path -> Array (Tuple String Int)
  largestSmallestPaths paths = [ outlier (\i j -> i > j) 0 paths, outlier (\i j -> i < j) maxSigned32BitInt paths ]
    where
    outlier :: (Int -> Int -> Boolean) -> Int -> Array Path -> Tuple String Int
    outlier criteria startValue paths' =
      foldl
        ( \acc p' ->
            ( case size p' of
                Just n -> if criteria n $ snd acc then Tuple (filename p') n else acc
                Nothing -> acc
            )
        )
        (Tuple "" startValue)
        paths'

allSizes :: Array Path -> Array (Tuple String Int)
allSizes paths =
  map
    ( \p -> case size p of
        Just n -> Tuple (filename p) n
        Nothing -> Tuple (filename p) 0
    )
    paths

whereIs :: String -> Maybe String
whereIs fileName = head $ whereIs' $ allFiles root
  where
  whereIs' :: Array Path -> Array String
  whereIs' paths = do
    path <- paths
    child <- ls path
    guard $ eq fileName $ fromMaybe "" $ last $ split (Pattern "/") $ filename child
    pure $ filename paths

