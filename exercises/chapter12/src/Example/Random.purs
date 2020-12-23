module Example.Random where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)
import Effect.Ref as Ref
import Effect.DOM (addEventListener, querySelector)
import Graphics.Canvas (strokePath, fillPath, arc, setStrokeStyle, setFillStyle, getContext2D, getCanvasElementById, Context2D, beginPath, strokePath, fill, rect, withContext, translate, rotate, scale)
import Example.Shapes
import Data.Int (toNumber)
import Math as Math
import Partial.Unsafe (unsafePartial)

{-
fillAndStrokePath :: forall a. Context2D -> Effect a -> Effect a
fillAndStrokePath ctx path = ???? do 
  fillPath ctx path
  strokePath ctx path

-}
fillAndStrokePath :: Context2D -> Effect Unit -> Effect Unit
fillAndStrokePath ctx path = void do 
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

  --fillAndStrokePath ctx path
  transformRotate ctx {x: 100.0, y: 200.0}

transformRotate :: Context2D -> Point -> Effect Unit
transformRotate ctx p = void do
  translate ctx { translateX: -p.x, translateY: -p.y }
  r <- random
  rotate ctx r
  translate ctx { translateX: p.x, translateY: p.y }  

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#F00"
  setStrokeStyle ctx "#000"

  for_ (1 .. 100) \_ -> do
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

    fillAndStrokePath ctx path

  node <- querySelector "#canvas"
  for_ node $ addEventListener "click" $ void do
    logShow "Mouse clicked!"
    render ctx