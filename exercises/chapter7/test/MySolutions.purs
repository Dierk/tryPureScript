module Test.MySolutions where

import Data.Maybe
import Prelude

import Control.Apply (lift2)
import Data.Either (either, fromRight)
import Data.FoldableWithIndex (foldMapWithIndexDefaultL)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)

import Data.AddressBook
import Data.AddressBook.Validation
import Data.Validation.Semigroup 

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

-- the validation use case is cool but at the same time
-- seems a bit overly complicated for such a simple task.
-- Where is the striking benefit? Compositionality of validations?

-- note pn.type can be written as pn."type" to avoid the reserved keyword restriction


failingRegex :: Regex
failingRegex = unsafePartial $ fromRight $ regex "(?!)" noFlags 

--- only for literal Strings
staticRegex :: String -> Regex
-- staticRegex str = unsafePartial $ fromRight $ regex str noFlags                    -- neither is nice
staticRegex str = either (const failingRegex) identity $ regex str noFlags

stateRegex :: Regex
stateRegex = staticRegex "^[a-zA-Z]{2,2}$"         -- no special escaping syntax?

nonEmptyRegex :: Regex
nonEmptyRegex = staticRegex "\\S"                  -- in other words: at least one non-space char


validateState :: String -> V Errors String
validateState state = matches "State" stateRegex state *> pure state

validateNonEmpty :: String -> String -> V Errors String
validateNonEmpty fieldName value = matches fieldName nonEmptyRegex value *> pure value

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> (validateNonEmpty "Street" a.street)
          <*> (validateNonEmpty "City"   a.city  )
          <*> (validateState a.state)