module Test.ObservableTest where


import Prelude
import Data.Traversable
import Control.Monad.ST (run)
import Effect (Effect)
import Observable (getEffects, getValue, newObservable, setValue)
import Test.Assert (assertEqual')
          


main :: Effect Unit
main = do    
    assertEqual' "initial value can be read" { 
        expected: 0, 
        actual: run (newObservable 0 >>= getValue)
        } 
        
    assertEqual' "changed value can be read" { 
        expected: 1, 
        actual: run (newObservable 0 >>= setValue 1 >>= getValue)
        }
    sequence_ $ run (newObservable 0 >>= setValue 1 >>= getEffects)
    pure unit
