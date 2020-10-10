module Test.MySolutions where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Picture (Point(..), Shape(..), origin)
import Math (pi)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--- n! / k! (n - k)!
binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial n k | n == k = 1
             | n <  k = 0
             | true   = factorial n / (factorial k * factorial(n-k))

pascal :: Int -> Int -> Int
pascal n k | n <  k = 0
           | n == 0 = 1
           | k == 0 = 1
           | true   = pascal (n-1) (k-1) + pascal (n-1) k             

--- not quite the most general but the most appropiate type (?)
sameCity :: forall p a. 
    { address :: {city :: String |a} |p} -> 
    { address :: {city :: String |a} |p} -> 
    Boolean
sameCity 
    { address: { city: cityA } } 
    { address: { city: cityB } } 
    = cityA == cityB            

fromSingleton :: forall a. a -> Array a -> a    
fromSingleton def [s] = s 
fromSingleton def  _  = def

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle    p r)   = Circle origin (r * 2.0)
doubleScaleAndCenter (Rectangle p w h) = Rectangle origin (w * 2.0) (h * 2.0)
doubleScaleAndCenter (Line (Point p1) (Point p2)) = 
    Line  (Point (normed p1)) (Point (normed p2)) where
        midx = (p1.x + p2.x) / 2.0
        midy = (p1.y + p2.y) / 2.0
        center {x, y} = {x: x - midx, y: y - midy}
        scale  {x, y} = {x: x * 2.0,  y: y * 2.0}
        normed = scale <<< center      
doubleScaleAndCenter (Text p s) = Text origin s
doubleScaleAndCenter (Clipped _ p w h) = doubleScaleAndCenter (Rectangle p w h)

shapeText :: Shape -> Maybe String
shapeText (Text p s) = Just s
shapeText _          = Nothing

area :: Shape -> Number
area (Circle p r)      = r * r * pi
area (Rectangle p w h) = w * h
area _                 = 0.0