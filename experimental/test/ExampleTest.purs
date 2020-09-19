module Test.ExampleTest where
  
import Prelude
import Effect (Effect)  
import Data.Array (reverse)
import Test.QuickCheck (quickCheck, (<?>), Result)  

addProperty :: Int -> Result
addProperty n = n + 1 > n <?> ("something wrong here with n:" <> show n)

reverseProperty :: Array Int -> Result
reverseProperty ary = ary == reverse (reverse ary) <?> ("reverse did not work for array:" <> show ary)

main :: Effect Unit
main = do
    quickCheck addProperty
    quickCheck reverseProperty