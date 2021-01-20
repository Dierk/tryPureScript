module Example.Shapes where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect, foreachE)
import Graphics.Canvas (Context2D, arc, closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, rect, setFillStyle, strokePath)
import Math (pow)
import Math as Math
import Partial.Unsafe (unsafePartial)

-- ANCHOR: translate
translate
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }
-- ANCHOR_END: translate

type Point = { x :: Number, y :: Number }

renderPath :: Context2D -> Array Point -> Effect Unit
renderPath ctx arr = void do
  strokePath ctx $ do
    foreachE arr (\p -> lineTo ctx p.x p.y)
    closePath ctx

f :: Number -> Point
f n = {x: n * 500.0 , y: (pow n 3.0) * 500.0 }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"

  fillPath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  setFillStyle ctx "#0F0"

  fillPath ctx $ arc ctx $ translate 200.0 200.0
    { x: 300.0
    , y: 300.0
    , radius: 50.0
    , start: 0.0
    , end: Math.tau * 2.0 / 3.0
    }

-- ANCHOR: path
  setFillStyle ctx "#F00"

  fillPath ctx $ do
    moveTo ctx 300.0 260.0
    lineTo ctx 260.0 340.0
    lineTo ctx 340.0 340.0
    closePath ctx
-- ANCHOR_END: path

  renderPath ctx [(f 0.99), (f 0.66), (f 0.33)]
