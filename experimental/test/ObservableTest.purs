module Test.ObservableTest where


import Data.Traversable
import Prelude

import Control.Monad.ST (run)
import Effect (Effect)
import Effect.Console (logShow)
import Observable 
import Test.Assert (assertEqual')

main :: Effect Unit
main = do    
    assertEqual' "initial value can be read" { 
        expected: 0, 
        actual: getValue $ newObservable 0
        } 
        
    plainObs <- setValue 1 $ newObservable 0    
    assertEqual' "changed value can be read" { 
        expected: 1, 
        actual: getValue plainObs 
        }
    logShow "this should log 1 and 2"
    oneObs <- setValue 1 $ onChange logShow (newObservable 0) 
    twoObs <- setValue 2 oneObs  

    logShow "this should log 1 and 2 (again, arguably nicer)"
    newObservable' 0
        >>= onChange' logShow
        >>= setValue 1
        >>= setValue 2
        >>= done

    pure unit 


