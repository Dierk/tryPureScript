module Example.LSystem where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (concatMap, foldM)
import Effect (Effect)
import Graphics.Canvas (strokePath, setStrokeStyle, lineTo, moveTo,
                        getContext2D, getCanvasElementById, fillPath,closePath, setShadowOffsetX, setShadowOffsetY, setShadowBlur, setShadowColor)
import Math as Math
import Partial.Unsafe (unsafePartial)

lsystem :: forall a m s. Monad m =>
                         Array a ->
                         (a -> Array a) ->
                         (s -> a -> m s) ->
                         Int ->
                         s -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s m = go (concatMap prod s) (m - 1)

lsystem1 :: forall a. Array a ->
                      (a -> Array a) ->
                      Int ->
                      Array a
lsystem1 s _ 0 = s                        
lsystem1 s prod n = lsystem1 (concatMap prod s) prod (n-1)

lsystem2 :: forall a m s. Monad m =>
                         Array a ->
                         (s -> a -> m s) ->
                         s -> m s
lsystem2 s interpret state = foldM interpret state s

{-type Angle = Number

data Letter1 = L Angle | R Angle | F

data Letter = L | R | F-}

data Letter2 = L | R | F | M

{-type Sentence1 = Array Letter

type Sentence = Array Letter-}

type Sentence2 = Array Letter2

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
   {- initial1 :: Sentence1
    initial1 = [F, (R (Math.tau / 6.0)), (R (Math.tau / 6.0)), F, (R (Math.tau / 6.0)), (R (Math.tau / 6.0)), F, (R (Math.tau / 6.0)), (R (Math.tau / 6.0))]

    initial :: Sentence
    initial = [F, R, R, F, R, R, F, R, R]-}

    initial2 :: Sentence2
    initial2 = [M]

   {- productions1 :: Letter1 -> Sentence1
    productions1 (L a) = [(L a)]
    productions1 (R a) = [(R a)]
    productions1 F = [F, (L (- (Math.tau / 6.0))), F, (R (Math.tau / 6.0)), (R (Math.tau / 6.0)), F, (L (- (Math.tau / 6.0))), F]

    productions :: Letter -> Sentence
    productions L = [L]
    productions R = [R]
    productions F = [F, L, F, R, R, F, L, F]-}

    productions2 :: Letter2 -> Sentence2
    productions2 L = [L]
    productions2 R = [R]
    productions2 F = [F,L,M,L,F,R,M,R,F,R,M,R,F,L,M,L,F]
    productions2 M = [M,R,F,R,M,L,F,L,M,L,F,L,M,R,F,R,M]

   {- interpret1 :: State -> Letter1 -> Effect State
    interpret1 state (L a) = pure $ state { theta = state.theta + a } -- - Math.tau / 6.0 }
    interpret1 state (R a) = pure $ state { theta = state.theta + a } -- + Math.tau / 6.0 }
    interpret1 state F = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      lineTo ctx x y
      pure { x, y, theta: state.theta }

    interpret :: State -> Letter -> Effect State
    interpret state L = pure $ state { theta = state.theta - Math.tau / 6.0 }
    interpret state R = pure $ state { theta = state.theta + Math.tau / 6.0 }
    interpret state F = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      lineTo ctx x y
      pure { x, y, theta: state.theta }-}

    interpret2 :: State -> Letter2 -> Effect State
    interpret2 state L = pure $ state { theta = state.theta - Math.tau / 6.0 }
    interpret2 state R = pure $ state { theta = state.theta + Math.tau / 6.0 }
    interpret2 state _ = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      lineTo ctx x y
      pure { x, y, theta: state.theta }


    initialState :: State
    initialState = { x: 120.0, y: 200.0, theta: 0.0 }

  setStrokeStyle ctx "#000"
  setShadowOffsetX ctx 10.0
  setShadowOffsetY ctx 40.0
  setShadowBlur ctx 4.0
  setShadowColor ctx "#444"

  --fillPath ctx $ lsystem initial productions interpret 5 initialState
  --fillPath ctx $ lsystem2 (lsystem1 initial productions 5) interpret initialState
  fillPath ctx $ lsystem2 (lsystem1 initial2 productions2 1) interpret2 initialState

