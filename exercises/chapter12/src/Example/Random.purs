module Example.Random where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.DOM (addEventListener, querySelector)
import Effect.Random (random)
import Example.Shapes (Point)
import Graphics.Canvas (Context2D, arc, fillPath, getCanvasElementById, getContext2D, rotate, setFillStyle, setStrokeStyle, strokePath, translate)
import Math as Math
import Partial.Unsafe (unsafePartial)

transformRotate :: Context2D -> Point -> Effect Unit
transformRotate ctx p = void do
  translate ctx { translateX: -p.x, translateY: -p.y }
  r <- random
  rotate ctx r
  translate ctx { translateX: p.x, translateY: p.y }

strokeAndFillPath :: Context2D -> Effect Unit -> Effect Unit
strokeAndFillPath ctx path = void do
  fillPath ctx path
  strokePath ctx path

render :: Context2D -> Effect Unit
render ctx = void do
  setFillStyle ctx "#F00"
  setStrokeStyle ctx "#000"
  x <- random
  y <- random
  r <- random

  let path = arc ctx
        { x     : x * 600.0
        , y     : y * 600.0
        , radius: r * 50.0
        , start : 0.0
        , end   : Math.tau
        }
  strokeAndFillPath ctx path


main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

-- ANCHOR: style
  setFillStyle ctx "#F00"
  setStrokeStyle ctx "#000"
-- ANCHOR_END: style

-- ANCHOR: for
  for_ (1 .. 100) \_ -> do
-- ANCHOR_END: for
-- ANCHOR: random
    x <- random
    y <- random
    r <- random
-- ANCHOR_END: random

-- ANCHOR: path
    let path = arc ctx
         { x     : x * 600.0
         , y     : y * 600.0
         , radius: r * 50.0
         , start : 0.0
         , end   : Math.tau
         }
    strokeAndFillPath ctx path

  node <- querySelector "#canvas"
  for_ node $ addEventListener "click" $ void do
    logShow "Mouse clicked!"
    render ctx
-- ANCHOR_END: path
