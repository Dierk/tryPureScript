module Test.MySolutions where

import Data.Picture
import Math
import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Person (Person)
import Data.Picture as DataP

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | k > n = 0
             | otherwise = (factorial n) / ((factorial k) * (factorial (n-k)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n-1) k + pascal (n-1) (k-1)

sameCity :: Person -> Person -> Boolean
sameCity {address: {city: x}} y = x == y.address.city

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [e] = e
fromSingleton d _ = d

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0 

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin (r*2.0)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (w*2.0) (h*2.0)
doubleScaleAndCenter (Line (Point s) (Point e)) =  
    let
        deltaX = (s.x + e.x) / 2.0
        deltaY = (s.y + e.y) / 2.0
    in
        (Line
            (Point { x: (s.x - deltaX) * 2.0, y: (s.y - deltaY) * 2.0 })
            (Point { x: (e.x - deltaX) * 2.0, y: (e.y - deltaY) * 2.0 })
        )
doubleScaleAndCenter (Text _ t) = Text origin t


shapeText :: Shape -> Maybe String
shapeText (Text _ t) = Just t
shapeText _ = Nothing

area :: Shape -> Number
area (Circle _ r) = r*r*pi
area (Rectangle _ w h) = w*h
area _ = 0.0

data ShapeExt
  = Clipped Picture Point Number Number
  | Shape Shape

shapeBounds :: ShapeExt -> Bounds
shapeBounds (Shape s) = DataP.shapeBounds s
shapeBounds (Clipped pic pt w h) = DataP.intersect (DataP.bounds pic) (DataP.shapeBounds (Rectangle pt w h))