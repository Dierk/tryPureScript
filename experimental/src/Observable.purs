module Observable where

import Prelude

import Control.Monad.ST (ST, run)
import Control.Monad.ST.Ref (STRef, modify, new, read, write)
import Data.Array (length)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Console (logShow)

type Observable a = {
    value :: a,
    effects :: Array (Effect Unit)
}
type ObservableRef r a = STRef r (Observable a)
type ObservableST  r a = ST r (Observable a)

newObservable :: forall a r. a ->  ST r (ObservableRef r a)  
newObservable val =  new { value : val, effects : [] }

wrapObservable :: forall a r. Observable a -> ST r (ObservableRef r a)
wrapObservable obs = new obs

getObservable :: forall a r. ObservableRef r a -> ObservableST r a
getObservable obsRef = read obsRef

withObservable :: forall a. 
    Observable a -> 
    (a -> a) ->
    -- the below would be nicer but then, "r" escapes its scope
    -- (forall r. (ObservableRef r a) -> ST r (ObservableRef r a)) ->
    Effect (Observable a)
withObservable obs modify = 
    let
        newObs = run ( wrapObservable obs >>= setValue (modify obs.value) >>= getObservable )
    in do
        sequence_ newObs.effects
        pure $ newObs {effects = []}

    

getValue :: forall a r. ObservableRef r a -> ST  r a
getValue obsRef = do
    obs    <- read obsRef
    pure   obs.value
    
getEffects :: forall a r. ObservableRef r a -> ST r (Array (Effect Unit))
getEffects obsRef = do
    obs    <- read obsRef
    pure   obs.effects    

setValue :: forall a r. a -> ObservableRef r a -> ST r (ObservableRef r a) 
setValue val obsRef = do 
    _ <- modify (\old -> {value: val, effects: old.effects <> [logShow $ "*** "<> show (length old.effects)]}) obsRef
    pure obsRef

{- ST functions
new    :: forall a r. a                      -> ST r (STRef r a)
read   :: forall a r. STRef r a              -> ST r a
write  :: forall a r. a         -> STRef r a -> ST r a
modify :: forall r a. (a -> a)  -> STRef r a -> ST r a
run    :: forall a. (forall r. ST r a) -> a

-}

{- considerations
   - make Observables an Effect and all operations effectful
   - make Observables an Effect and the internals ST-ful
   - make Observables immutable wrap them into ST
   - notification callbacks will need to be effectful
   - must 2 Observables share the same region ? 
        obs1 reads/writes to obs2
        obs1 reads/writes to itself
-}