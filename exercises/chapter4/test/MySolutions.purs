module Test.MySolutions where


import Prelude

import Control.MonadZero (guard)
import Data.Array (length, filter, (..), cons, (:), head, tail, null, last)
import Data.Foldable (foldr, product, foldl)
import Data.Int (rem, quot)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Path (Path, root, size, filename, isDirectory, ls)
import Data.Tuple (Tuple(..), snd)
import Test.Examples (factors, allFiles)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n -2)

isTrue :: Boolean -> Int
isTrue true = 1
isTrue false = 0

countEven :: Array Int -> Int
countEven arr = foldr (+) 0 (map (\x -> isTrue (isEven x)) arr)

squared :: Array Number -> Array Number
squared = map (\x -> x*x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\n -> n >= 0.0)

infix 10 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\n -> n >= 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime n = n > 1 && (length (factors n)) == 1

cartesianProduct ::forall a. Array a-> Array a-> Array (Array a)
cartesianProduct xs ys = do
    x <- xs
    y <- ys
    pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a*a + b*b == c*c
  pure [a,b,c]

factorize :: Int -> Array Int
factorize n = factorize' 2 n []
  where 
  factorize' :: Int -> Int -> Array Int -> Array Int
  factorize' _ 1 result = result
  factorize' divisor n result = 
    let 
        remainder = rem n divisor
    in 
        if remainder == 0 then 
            factorize' divisor (quot n divisor) (cons divisor result )
        else 
            factorize' (divisor+1) n result

allTrue :: Array Boolean -> Boolean
allTrue = foldl (==) true


fibTailRec :: Int -> Int
fibTailRec n = fibTailRec' n 0 0 1
  where
  fibTailRec' :: Int -> Int -> Int -> Int -> Int
  fibTailRec' limit count n1 n2 = 
    if limit == count then  
        n1 + n2
    else
        fibTailRec' limit (count +1) (n1+n2) n1


reverse :: forall a. Array a -> Array a
reverse = foldl  (\xs x -> [ x ] <> xs)  []

onlyFiles :: Path -> Array Path
onlyFiles file =
    if isDirectory file == true then
        do
            child <- ls file
            onlyFiles child
    else
        file : do
            child <- ls file
            onlyFiles child



largestSmallest :: Path -> Array (Tuple String Int)
largestSmallest file = 
    let files = allSizes (onlyFiles file)
    in
        [largest files, smallest files]
        where
            largest = foldl (\x y -> if snd x > snd y then x else y ) (Tuple "no" 0)
            smallest = foldl (\x y -> if snd x < snd y then x else y ) (Tuple "no" 2147483647)


allSizes :: Array Path -> Array (Tuple String Int)
allSizes paths =
  map
    ( \p -> case size p of
        Just n -> Tuple (filename p) n
        Nothing -> Tuple (filename p) 0
    )
    paths

whereIs :: String -> Maybe String 
whereIs name = rec root
    where
        rec file = 
            if null (find file) then
                if isDirectory file then
                    foldr (\x y -> if x == Nothing then y else x) Nothing (map rec (ls file))
                else 
                    Nothing                     
            else
                Just (filename file)
        find file = filter (\x -> fromMaybe "" (last (split (Pattern "/") (filename x))) == name) (ls file) 