module Test.Main where
  
import Prelude
import Effect (Effect)
import Test.BalancedParensKmettTest as Kmett
import Test.BalancedParensScanTest  as Scan
import Test.ExampleTest             as Example

main :: Effect Unit
main = do
  Kmett.main   
  Scan.main  
  Example.main