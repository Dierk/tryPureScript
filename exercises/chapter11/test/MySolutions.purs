module Test.MySolutions where

import Control.Monad.State
import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (Reader, ReaderT, ask, lift, local, runReader, runReaderT)
import Control.Monad.State (State, StateT, get, put, execState, modify_)
import Control.Monad.Writer (Writer, WriterT, tell, runWriter, execWriterT)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (joinWith, take, drop)
import Data.String.CodeUnits (stripPrefix, toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence, traverse_)
import Data.Tuple (Tuple, fst, snd)
import Control.Monad.Except.Trans
import Control.Monad.Writer.Trans

sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

testExecState :: Int
testExecState = execState (do
    sumArray [1, 2, 3]
    sumArray [4, 5]
    sumArray [6]) 0

testEvalState :: Unit
testEvalState = evalState (do
    sumArray [1, 2, 3]
    sumArray [4, 5]
    sumArray [6]) 0

testRunState :: Tuple Unit Int
testRunState = runState (do
    sumArray [1, 2, 3]
    sumArray [4, 5]
    sumArray [6]) 0


-- peek
testParens :: String -> Boolean
testParens str = 
  let
    openTally :: Char -> Int -> Int
    -- Open parens only considered if not already in deficit.
    -- No recovery from too-many closed parens.
    openTally '(' tally | tally >= 0 = tally + 1
    openTally ')' tally = tally - 1
    -- Non-parens has no effect
    openTally _ tally = tally

    sumParens :: Array Char -> State Int Unit
    sumParens = traverse_ \c -> modify_ $ openTally c

    finalTally :: Int
    finalTally = execState (sumParens $ toCharArray str) 0
  in
    finalTally == 0

type Level = Int

type Doc = Reader Level String

--peek
line :: String -> Doc
line s = do
  level <- ask
  pure $ (power "  " level) <> s

indent :: Doc -> Doc
indent = local $ (+) 1

-- peek
cat :: Array Doc -> Doc
-- sequence :: forall a m. Applicative m => t (m a) -> m (t a) 
-- (>=>) composeKleisli :: forall a b c m. Bind m => (a -> m b) -> (b -> m c) -> a -> m c
-- (>>>) composeFlipped :: forall a b c d. Semigroupoid a => a b c -> a c d -> a b d
cat = sequence >=> joinWith "\n" >>> pure

render :: Doc -> String
render doc = runReader doc 0

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \n -> do
  tell (Additive n)
  pure unit


collatz :: Int -> Tuple Int (Array Int) 
-- runWriter  :: Writer (Array Int) Int -> Tuple Int (Array Int)
collatz c = runWriter (cltz 0 c)
  where
    cltz :: Int -> Int -> Writer (Array Int) Int
    cltz i 1 =  do
      tell [ 1 ]
      pure i
    cltz i n = do
      tell [ n ]
      if mod n 2 == 0
        then cltz (i+1) (n/2)
        else cltz (i+1) ((3 * n) + 1)

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split :: Parser String
split = do
  s <- get
  lift $ tell ["The state is " <> s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

safeDivide :: Int -> Int -> (ExceptT String Identity) Int
safeDivide n d = do 
  case d of
    0 -> throwError "denominator is Zero"
    _ -> pure (n / d)
{-
> runExceptT (safeDivide 6 3)
(Identity (Right 2))
> runExceptT (safeDivide 6 0)
(Identity (Left "denominator is Zero"))
-}

runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

string :: String -> Parser String
string prefix = do
  s <- get
  lift $ tell ["The state is " <> s]
  case stripPrefix (Pattern prefix) s of
    Nothing -> lift $ lift $ throwError ["Could not parse"]
    (Just suffix) -> do 
      put suffix
      pure prefix


--peek
type Level' = Int
type Doc' = WriterT (Array String) (ReaderT Level' Identity) Unit

--peek
line' :: String -> Doc'
line' s = do
  level <- lift $ ask
  tell [(power "  " level) <> s]
  pure unit

indent' :: Doc' -> Doc'
indent' = local $ (+) 1

render' :: Doc' -> String
render' doc = joinWith "\n" $ unwrap $ runReaderT (execWriterT doc) 0