module Example.Rectangle where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (arc, fillPath, getCanvasElementById, getContext2D, moveTo, rect, rotate, setFillStyle, setStrokeStyle, strokePath, translate)
import Math as Math
import Partial.Unsafe (unsafePartial)

-- ANCHOR: main
main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
-- ANCHOR_END: main

-- ANCHOR: setFillStyle
  setFillStyle ctx "#0FA"
  setStrokeStyle ctx "#0AA"
-- ANCHOR_END: setFillStyle

-- ANCHOR: fillPath
  fillPath ctx $ do
   let
     rectangle = { x: 250.0
                 , y: 250.0
                 , width: 100.0
                 , height: 100.0
                 }
   translate ctx { translateX: -200.0, translateY: -200.0 }
   rect ctx rectangle
   translate ctx { translateX: 200.0, translateY: 200.0 }
   rect ctx rectangle
   moveTo ctx 500.0 500.0
   arc ctx
    { x: 500.0
    , y: 500.0
    , radius: 50.0
    , start: 1.0
    , end: Math.tau * 1.0 / 2.0
    }



-- ANCHOR_END: fillPath
