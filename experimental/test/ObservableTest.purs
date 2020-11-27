module Test.ObservableTest where


import Observable (getValue, newObservable)
import Control.Monad.ST (run)
import Prelude
import Effect (Effect)

import Test.Assert (assertEqual')
          

main :: Effect Unit
main = do    
    assertEqual' "initial value can be read" { 
        expected: 0, 
        actual: run (getValue $ newObservable 0)
        }