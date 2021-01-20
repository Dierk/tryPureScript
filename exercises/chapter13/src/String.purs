module LowerCaseString where

import Prelude

import Data.Enum (enumFromTo)
import Data.NonEmpty (oneOf)
import Data.String.CodeUnits (fromCharArray)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (arrayOf, elements)

newtype LowerCaseString = LowerCaseString String


instance arbitrary :: Arbitrary LowerCaseString where
  arbitrary = do
    let xs = enumFromTo 'a' 'z'
    xs' <- arrayOf $ elements xs
    pure $ LowerCaseString $ fromCharArray $ oneOf xs

instance showLowerCaseString :: Show LowerCaseString where
  show (LowerCaseString s) = show s