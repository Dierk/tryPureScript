module Test.MySolutions where

import Prelude

import Control.Alt (alt)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Function.Uncurried (Fn3)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Pair (Pair(..))
import Data.Set (Set)
import Test.Examples (Complex, Quadratic)

foreign import volumeFn :: Fn3 Number Number Number Number

foreign import volumeArrow :: Number -> Number -> Number -> Number

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsImpl :: (forall a. a -> a -> Pair a) -> Quadratic -> Pair Complex

foreign import valuesOfMapImpl :: Json -> Json

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair

valuesOfMap :: Map String Int -> Either String (Set Int)
valuesOfMap = decodeJson <<< valuesOfMapImpl <<< encodeJson

valuesOfMapGeneric :: forall k v.
  EncodeJson k =>
  Ord k        =>
  EncodeJson v =>
  DecodeJson v =>
  Ord v        =>
  Map k v -> Either String (Set v)
valuesOfMapGeneric = decodeJson <<< valuesOfMapImpl <<< encodeJson

foreign import quadraticRootsSetImpl :: Quadratic -> Json

quadraticRootsSet :: Quadratic -> Either String (Set Complex)
quadraticRootsSet = decodeJson <<< quadraticRootsSetImpl

newtype WrapPair a
  = WrapPair (Pair a)

instance decodeJsonWrapPair :: DecodeJson a => DecodeJson (WrapPair a) where
  decodeJson j = do
    decoded <- decodeJson j
    case decoded of
      [ a, b ] -> WrapPair <$> (Pair <$> (decodeJson a) <*> (decodeJson b))
      _ -> Left "Couldn't decode WrapPair"

foreign import quadraticRootsSafeImpl :: Quadratic -> Json

quadraticRootsSafe :: Quadratic -> Either String (Pair Complex)
quadraticRootsSafe q = (\(WrapPair p) -> p) <$> quadraticRootsSafeWrap q

quadraticRootsSafeWrap :: Quadratic -> Either String (WrapPair Complex)
quadraticRootsSafeWrap = decodeJson <<< quadraticRootsSafeImpl

decodeArray2D :: String -> Either String (Array (Array Int))
decodeArray2D s = do
  json  <- jsonParser s
  decodeJson json

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)
derive instance genericTree :: Generic (Tree a) _

instance encodeJsonTree :: EncodeJson a => EncodeJson (Tree a) where
  encodeJson t = genericEncodeJson t

instance decodeJsonTree :: DecodeJson a => DecodeJson (Tree a) where
  decodeJson t = genericDecodeJson t

instance eqTree :: Eq a => Eq (Tree a) where
  eq t = genericEq t

instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t

data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

instance encodeJsonIntOrString :: EncodeJson IntOrString where
  encodeJson (IntOrString_Int i) = encodeJson i
  encodeJson (IntOrString_String s) = encodeJson s

instance decodeJsonIntOrString :: DecodeJson IntOrString where
  decodeJson j =
    foldr alt (Left "Could not decode IntOrString")
      [ map IntOrString_Int $ decodeJson j
      , map IntOrString_String $ decodeJson j
      ]

derive instance genericIntOrString :: Generic IntOrString _

instance eqIntOrString :: Eq IntOrString where
  eq = genericEq

instance showIntOrString :: Show IntOrString where
  show = genericShow