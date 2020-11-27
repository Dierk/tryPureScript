module Test.ObservableTest where


import Prelude
import Data.Traversable
import Control.Monad.ST (run)
import Effect (Effect)
import Observable (getObservable, getEffects, getValue, newObservable, setValue, withObservable)
import Test.Assert (assertEqual')
import Effect.Console (logShow)
          

observedSetValue x = 
    let 
        obs = run (newObservable 0 >>= setValue x >>= getObservable)
    in do
        logShow $ obs.value              
    

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

    observedSetValue 1

    xxx <- withObservable {value:42, effects:[]} \x -> x+1
    yyy <- withObservable xxx                    \x -> x+1

    pure unit
