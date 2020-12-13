module Test.MySolutions where

import Prelude

import Data.String(joinWith)
import Data.Foldable (traverse_)
import Data.Identity
import Data.Monoid (power, (<>))
import Data.Traversable (sequence)
import Control.Monad.State
import Control.Monad.State.Class

import Control.Monad.Reader

{-  State functions:
    get     :: forall s.             State s s
    gets    :: forall s. (s -> a) -> State s a
    put     :: forall s. s        -> State s Unit
    modify  :: forall s. (s -> s) -> State s s
    modify_ :: forall s. (s -> s) -> State s Unit

    evalState :: forall s a. State s a -> s -> a
    execState :: forall s a. State s a -> s -> s
    runState  :: forall s a. State s a -> s -> Tuple a s
-}

-- traverse_ :: ∀ m. Applicative m => (Int -> m Int) -> Array Int -> m Unit
-- here, m is `State Int`, a type of kind Type -> Type
sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

-- type inference gives us the first monad transformer
-- stateExample :: ∀ a. (StateT Int Identity Unit -> Int -> a) -> a
stateExample :: ∀ a. (State Int Unit -> Int -> a) -> a
stateExample stateFunction = stateFunction (do
    sumArray [1, 2, 3]
    sumArray [4, 5]
    sumArray [6]) 0

-- ----- Reader Example -----

type Level = Int
type Doc   = Reader Level String

-- "renders a function" ???
line :: String -> Doc
line str = do
    level <- ask
    pure $ (power " " level) <> str

indent :: Doc -> Doc
indent = local (_ + 1)

-- sequence :: forall a m . Applicative m => Array (m a) -> m (Array a)

cat :: Array Doc -> Doc
cat docs = do
    lines <- sequence docs
    pure $ joinWith "\n" lines

render :: Doc -> String
render doc = runReader doc 0

exampleDoc = render $ cat
   [ line "Here is some indented text:"
   , indent $ cat
       [ line "I am indented"
       , line "So am I"
       , indent $ line "I am even more indented"
       ]
   ]
