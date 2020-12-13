module Test.MySolutions where

import Prelude

import Data.String(joinWith)
import Data.String.CodeUnits(stripPrefix)
import Data.String.Pattern
import Data.Maybe
import Data.Int(even, quot)
import Data.Foldable (traverse_)
import Data.Identity
import Data.Tuple (Tuple, snd)
import Data.Monoid (power, (<>))
import Data.Monoid.Additive
import Data.Traversable (sequence)
import Control.Monad.State
import Control.Monad.State.Class

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except.Trans

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

-- ----- Writer -----

-- traverse_ :: ∀ m. Applicative m => (Int -> m Int) -> Array Int -> m Unit
-- here, m is `Writer (Additive Int)`, a type of kind Type -> Type
sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \n -> do
    tell (Additive n)
    pure unit

writerExample :: Int
writerExample = unwrapAdditive $ snd $ runWriter (do
    sumArrayWriter [1, 2, 3]
    sumArrayWriter [4, 5]
    sumArrayWriter [6])

unwrapAdditive :: ∀ a. Additive a → a
unwrapAdditive (Additive x) = x

collatz :: Int -> Int -> Int
collatz n counter =
    if (1 == n)
    then counter
    else if (even n)
         then collatz (n `quot` 2) (counter + 1)
         else collatz ( 3 * n + 1) (counter + 1)

collatzW :: Int -> Int -> Writer (Array Int) Int
collatzW n counter = do -- the counter is now redundant
    tell [n]
    if (1 == n)
    then pure counter
    else if (even n)
         then collatzW (n `quot` 2) (counter + 1)
         else collatzW ( 3 * n + 1) (counter + 1)

runCollatzW n = runWriter $ collatzW n 0

-- ----- Monad Transformers ------

safeDivide :: Int -> Int -> ExceptT String (Identity) Int
safeDivide num denom =
    if (denom == 0)
    then throwError "cannot divide by zero"
    else pure (num `quot` denom)

unwrapIdentity (Identity x) = x

runSafeDivide num denom = unwrapIdentity $ runExceptT $ safeDivide num denom


type Errors = Array String
type Log    = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

string :: String -> Parser String
string prefix = do
    value <- get     -- getting the value of the state
    lift $ tell ["The state is '" <> value <> "'."]
    case stripPrefix (Pattern prefix) value of
        Nothing       -> lift $ lift $ throwError ["No prefix '" <> prefix <> "' in '" <> value <> "'."]
        Just stripped -> do
            put stripped
            pure prefix

runParser p s = unwrapIdentity $ runExceptT $ runWriterT $ runStateT p s

-- ----- document indentation with monad transformers -----

type DocT = ReaderT Level (Writer (Array String)) Unit

-- alternative version with an inner WriterT to follow the exercise description more closely
-- type DocT = ReaderT Level (WriterT (Array String) Identity ) Unit
-- renderT doc = joinWith "\n" $ snd $ unwrapIdentity $ runWriterT $ runReaderT doc 0

lineT :: String -> DocT
lineT str = do
    level <- ask
    lift $ tell $ [power " " level <> str]

indentT :: DocT -> DocT
indentT = local (_ + 1)

renderT :: DocT -> String
renderT doc = joinWith "\n" $ snd $ runWriter $ runReaderT doc 0

exampleDocT :: DocT
exampleDocT = do
   lineT "Here is some indented text:"
   indentT do
       lineT "I am indented"
       lineT "So am I"
       indentT do
           lineT "I am even more indented"

