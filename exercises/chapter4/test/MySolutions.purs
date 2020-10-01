module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concat, filter, head, tail, reverse, sort, (..))
import Data.Int (floor, quot, toNumber)
import Data.Maybe (Maybe, fromMaybe)
import Math (sqrt)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = false == isEven (n-1)

countEven :: Array Int -> Int
countEven [] = 0
countEven ns = count + (countEven $ fromMaybe [] (tail ns)) where 
    count = if isEven (fromMaybe 1 (head ns)) then 1 else 0

squared :: Array Number -> Array Number
squared ns = map square ns where
    square n = n * n      

keepNonNegative :: Array Number -> Array Number
keepNonNegative ns = filter nonNegative ns where
  nonNegative n = false == (n < 0.0)

infixl 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite ns = (_ >= 0.0) <$?> ns

myFactors :: Int -> Array (Array Int)
myFactors n = do
  x <- 1 .. (floor $ sqrt $ toNumber n)
  let y = quot n x 
  guard $ x * y == n 
  pure [x, y]

isPrime :: Int -> Boolean
isPrime 1 = false
isPrime n = myFactors n == [[1,n]]  

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
    x <- xs
    y <- ys
    pure [x, y]

triples :: Int -> Array (Array Int)
triples upperlimit = do
  n <- 1 .. upperlimit 
  x <- 1 .. n
  y <- x .. n 
  guard $ x*x + y*y == n*n 
  pure [x,y,n]   

highestPrimeFactor :: Int -> Maybe Int
highestPrimeFactor n = head $ reverse $ sort primes where
    flatFactors = concat $ myFactors n
    primes      = filter isPrime flatFactors

factorize :: Int -> Array Int 
factorize 1 = []
factorize n = if isPrime n 
    then [n]
    else [high] <> factorize (quot n high) where
      high = fromMaybe n $ highestPrimeFactor n 
