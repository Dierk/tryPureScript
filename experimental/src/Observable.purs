module Observable where

import Prelude
import Effect(Effect)

import Control.Monad.ST.Ref (STRef, modify, new, read)
import Control.Monad.ST (ST, for, run)

type Observable a = {
    value :: a
}
type ObservableRef r a = STRef r (Observable a)

newObservable :: forall a r. a ->  ST r (ObservableRef r a)  
newObservable val =  new { value : val }

getValue :: forall a r. ST r (ObservableRef r a) -> ST r a
getValue observable = do
    obsRef <- observable 
    obs    <- read obsRef
    pure   obs.value


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