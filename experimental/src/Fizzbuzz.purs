module Fizzbuzz where

import Effect
import Effect.Console (log)
import Data.List.Lazy (take, zipWith, fromFoldable, cycle, iterate, foldr)
import Data.Monoid (mempty, (<>))
import Data.Maybe (Maybe(..), fromMaybe)

import Prelude ( Unit, show, map, ($), (+))

main :: Effect Unit
main = do
    log $ join $ take 100 $ fizzbuzz
  where
    join = foldr (\x y -> x <> " " <> y) ""

    nums     = map show $ iterate (\x -> x+1) 1
    fizzes   = cycle $ fromFoldable [mempty, mempty, Just "fizz" ]
    buzzes   = cycle $ fromFoldable [mempty, mempty, mempty, mempty, Just "buzz" ]
    pattern  = zipWith (<>)      fizzes buzzes
    fizzbuzz = zipWith fromMaybe nums   pattern
