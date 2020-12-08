module Observable where

import Prelude

import Data.Array (snoc)
import Data.Foldable (sequence_)
import Effect (Effect)


type ValueChangeListener a = ( a -> Effect Unit)

type Observable a =
    { value     :: a
    , listeners :: Array (ValueChangeListener a)
    }
 
newObservable :: forall a. a -> Observable a
newObservable val = {value: val, listeners: []}

getValue :: forall a. Observable a -> a 
getValue obs = obs.value

setValue :: forall a. Eq a => a -> Observable a -> Effect (Observable a)
setValue newValue obs =
    if ( obs.value == newValue ) -- shortcircuit on same value
        then do
            pure obs
        else do
            sequence_ $ map (\listener -> listener newValue) obs.listeners
            pure obs {value = newValue}

onChange :: forall a. ValueChangeListener a -> Observable a -> Observable a 
onChange listener obs = obs {listeners = snoc obs.listeners listener}

-- ----------------- convenience Effect wrapper ------------------------

newObservable' :: forall a. a -> Effect (Observable a)
newObservable' val = pure $ newObservable val

onChange' :: forall a. ValueChangeListener a -> Observable a -> Effect(Observable a) 
onChange' li obs = pure $ onChange li obs

done :: forall a. Observable a -> Effect Unit
done _ = pure unit


{- considerations
   - make Observables an Effect and all operations effectful
   - make Observables an Effect and the internals ST-ful
   - make Observables immutable wrap them into ST
   - notification callbacks will need to be effectful
   - must 2 Observables share the same region ? 
        obs1 reads/writes to obs2
        obs1 reads/writes to itself
-}
