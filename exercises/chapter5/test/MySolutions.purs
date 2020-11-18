module Test.MySolutions where

import Prelude

factorial :: Int -> Int
factorial 0 = 1
factorial n = factorial (n - 1) * n

binomial :: Int -> Int -> Int
binomial n 0          = 1
binomial n 1          = n
binomial n k | n == k = 1
             | n < k  = 0
             | otherwise = factorial n / (factorial k * (factorial $ n - k))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal n k = binomial (n - 1) k + binomial (n - 1) (k - 1)

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [b] = b
fromSingleton a _ = a