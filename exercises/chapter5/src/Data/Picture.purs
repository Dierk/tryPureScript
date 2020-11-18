module Data.Picture where

import Data.Maybe
import Data.Tuple
import Prelude

import Data.Foldable (foldl)
import Global as Global
import Math (pi)
import Math as Math
import Test.QuickCheck.Gen (sample)

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture Point Number Number

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
showShape (Clipped _ c w h) = showShape $ Rectangle c w h

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

scaleShape :: Number -> Shape -> Shape
scaleShape i (Circle c r) = Circle c (r * i)
scaleShape i (Rectangle c w h) = Rectangle c (w * i) (h * i)
scaleShape i (Line (Point s) (Point e)) =
  (Line
    (Point { x: s.x * i, y: s.y * i })
    (Point { x: e.x * i, y: e.y * i })
  )
scaleShape i text = text

centerShape :: Shape -> Shape
centerShape (Circle c r) = Circle origin r
centerShape (Rectangle c w h) = Rectangle origin w h
centerShape (Line (Point s) (Point e)) =
  (Line
    (Point { x: -deltaX, y: -deltaY })
    (Point { x: deltaX, y: deltaY })
  )
  where
  deltaX = (e.x - s.x) / 2.0
  deltaY = (e.y - s.y) / 2.0
centerShape (Text loc text) = Text origin text
centerShape (Clipped _ c w h) = centerShape $ Rectangle c w h

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _          = Nothing

origin :: Point
origin = Point { x: 0.0, y: 0.0 }

getCenter :: Shape -> Point
getCenter (Circle c r) = c
getCenter (Rectangle c w h) = c
getCenter (Line (Point s) (Point e)) = Point { x: s.x - e.x, y: s.y - e.y }
getCenter (Text loc text) = loc
getCenter (Clipped _ c w h) = getCenter $ Rectangle c w h

area :: Shape -> Number
area (Circle _ r)      = pi * r * r
area (Rectangle _ w h) = w * h
area _                 = 0.0

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped p c w h) = intersect (bounds p) (shapeBounds $ Rectangle c w h)


union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

{-
These `instance`s are to enable testing.
Feel free to ignore these.
They'll make more sense in the next chapter.
-}
derive instance boundsEq :: Eq Bounds

instance boundsShow :: Show Bounds where
  show b = showBounds b

derive instance pointEq :: Eq Point

instance pointShow :: Show Point where
  show p = showPoint p

derive instance shapeEq :: Eq Shape

instance shapeShow :: Show Shape where
  show = showShape