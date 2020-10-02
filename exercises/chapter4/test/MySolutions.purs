module Test.MySolutions where

import Data.Path
import Data.Tuple
import Prelude
import Test.Examples

import Control.MonadZero (guard)
import Data.Array (concat, elem, filter, foldl, head, reverse, sort, sortWith, tail, (..))
import Data.Int (floor, quot, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

-- foldl (==) false xs   -- returns true when xs = [false], [false, true, false], [false, true, false, true, false]  

fibTailRec :: Int -> Int
fibTailRec 0 = 1   
fibTailRec 1 = 1  
fibTailRec n = ftrI 2 1 1 where  
   ftrI x min2 min1 =
     if (x == n)
     then min2 + min1
     else ftrI (x+1) min1 (min2 + min1) 

myreverse :: forall a. Array a -> Array a
myreverse = foldl (\acc elem -> [elem] <> acc) []       

onlyFiles :: Path -> Array Path
onlyFiles path = filter (\p -> false == isDirectory p) $ allFiles path

largestSmallest :: Path -> Array (Tuple String Int)
largestSmallest path = [ largest, smallest ] where
  files             = onlyFiles path
  sortCriterion p   = fromMaybe 0 (size p)
  sortedPaths       = sortWith sortCriterion files
  pop paths         = fromMaybe path $ head paths
  smallest          = Tuple (filename $ pop sortedPaths) 0
  largest           = Tuple (filename $ pop $ reverse sortedPaths)  0

whereIs :: Path -> String -> Maybe String
whereIs dir name = 
  let filesInDir = (\path -> false == isDirectory path) <$?> ls dir 
      dirsInDir  = (\path -> true  == isDirectory path) <$?> ls dir    
  in  if (filename dir <> name) `elem` (filename <$> filesInDir)
      then Just (filename dir)
      else let descentMaybes = (\subdir -> whereIs subdir name) <$> dirsInDir    
               descentJusts  = (\mb -> mb /= Nothing) <$?>  descentMaybes  
           in  fromMaybe Nothing $ head descentJusts