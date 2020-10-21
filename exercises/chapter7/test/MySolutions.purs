module Test.MySolutions where

import Data.Maybe
import Prelude

import Control.Apply 

-- lift3 is well motivated. Note that 'map' also lifts.
-- <> $ <$> <*>
-- "apply" makes cool use of curried style plus right associativity of -> function definition
-- which makes the <*> operation monoidal.

-- "new, larger language" is a bit over the top. I actually find it more a sublanguage, an internal DSL.
-- ado .. in
-- Applicative "effect" or "side-effect" is not well motivated. sequence, caching, ...?

-- addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
-- addMaybe (Just x) (Just y) = Just (x+y)  -- nicely shows that this is a lifting of (+)
-- addMaybe  _        _       = Nothing 

addMaybe :: forall m. Apply m => m Int -> m Int -> m Int
addMaybe = lift2 (+)
subMaybe = lift2 (-)
mulMaybe = lift2 (*)
divMaybe = lift2 (/)

addApply = lift2 (+)
subApply = lift2 (-)
mulApply = lift2 (*)
divApply = lift2 (/)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe  Nothing  = pure Nothing
combineMaybe (Just fa) = Just <$> fa
-- combineMaybe (Just fa) = pure Just <*> fa        -- works as well
