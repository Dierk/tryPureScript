module Test.MySolutions where

import Prelude
import Data.Function.Uncurried (Fn3, mkFn3)
import Test.Examples
import Data.Pair (Pair(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser)
import Data.Map (Map)
import Data.Set (Set)
import Data.Either (Either(..))
import Control.Apply (lift2)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Foldable (foldr)
import Control.Alt (alt)


foreign import volumeFn :: Fn3 Number Number Number Number

--volumeFn :: Fn3 Number Number Number Number
--volumeFn = mkFn3 \l w h -> l * w * h

foreign import volumeArrow :: Number -> Number -> Number -> Number

--volumeArrow :: Number -> Number -> Number -> Number
--volumeArrow l w h = l * w * h

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsImpl :: (forall a. a -> a -> Pair a) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots q = quadraticRootsImpl Pair q

foreign import valuesOfMapJson :: Json -> Json

valuesOfMap :: Map String Int -> Either String (Set Int)
valuesOfMap = encodeJson >>> valuesOfMapJson >>> decodeJson

valuesOfMapGeneric :: forall k v. 
  EncodeJson k => 
  EncodeJson v => 
  DecodeJson v =>
  Ord k => 
  Ord v =>
  Map k v -> Either String (Set v)
valuesOfMapGeneric = encodeJson >>> valuesOfMapJson >>> decodeJson

foreign import quadraticRootsSetJson :: Json -> Json

quadraticRootsSet :: Quadratic -> Either String (Set Complex)
quadraticRootsSet = encodeJson >>> quadraticRootsSetJson >>> decodeJson

foreign import quadraticRootsSafeJson :: Json -> Json

quadraticRootsSafeWrap :: Quadratic -> Either String (WrapPair Complex)
quadraticRootsSafeWrap = encodeJson >>> quadraticRootsSafeJson >>> decodeJson

newtype WrapPair a
  = WrapPair (Pair a)

instance decodeJsonWrapPair :: DecodeJson a => DecodeJson (WrapPair a) where
  decodeJson j = do
    decoded <- decodeJson j
    case decoded of
      [ a, b ] -> map WrapPair $ lift2 Pair (decodeJson a) (decodeJson b)
      _ -> Left "Couldn't decode WrapPair"

quadraticRootsSafe :: Quadratic -> Either String (Pair Complex)
quadraticRootsSafe = quadraticRootsSafeWrap >>> map (\(WrapPair p) -> p)

decodeArray2D :: String -> Either String (Array (Array Int))
decodeArray2D str = do
  j <- jsonParser str
  decodeJson j

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

derive instance genericIntOrString :: Generic IntOrString _

instance eqIntOrString :: Eq IntOrString where
  eq t = genericEq t

instance showIntOrString :: Show IntOrString where
  show t = genericShow t

instance encodeJsonIntOrString :: EncodeJson IntOrString where
  encodeJson (IntOrString_Int i) = encodeJson i
  encodeJson (IntOrString_String s) = encodeJson s

instance decodeJsonIntOrString ::  DecodeJson IntOrString where
  decodeJson j =
    foldr alt (Left "Could not decode IntOrString")
      [ map IntOrString_Int $ decodeJson j
      , map IntOrString_String $ decodeJson j
      ]

