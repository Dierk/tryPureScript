module Test.MySolutions where

import Control.Monad.State
import Control.Monad.State.Class
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (class MonadError, Except, ExceptT(..), runExceptT, throwError)
import Control.Monad.Reader (Reader, ReaderT(..), ask, local, runReader, runReaderT)
import Control.Monad.Writer (Writer, WriterT(..), execWriterT, runWriter, runWriterT, tell)
import Control.MonadPlus (guard)
import Data.Array (many, some)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), drop, joinWith, length, stripPrefix, take, toLower, toUpper)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)

sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n

a = execState (do
  sumArray [1, 2, 3]
  sumArray [4, 5]
  sumArray [6]) 0 {- 21 -}

b = evalState (do
  sumArray [1, 2, 3]
  sumArray [4, 5]
  sumArray [6]) 0 {- unit -}

c = runState (do
  sumArray [1, 2, 3]
  sumArray [4, 5]
  sumArray [6]) 0 {- (21, unit) -}

countBalancedParens :: Array Char -> State Int Unit
countBalancedParens = traverse_ \char -> modify \count -> case char of
  '(' -> if count >= 0 then count + 1 else count
  ')' -> count - 1
  _   -> count

testParens :: String -> Boolean
testParens s =
  let
    parensCount = execState (countBalancedParens $ toCharArray s) 0
  in
  parensCount == 0

type Level = Int
type Doc = Reader Level String

line :: String -> Doc
line s = do
  lvl <- ask
  pure $ (power "  " lvl) <> s

indent :: Doc -> Doc
indent = local $ \lvl -> lvl + 1

{- sequence :: Array ((Reader Level) String) -> (Reader Level) Array String)) -}
cat :: Array Doc -> Doc
cat arrayDocs = do
  arrayString <- sequence arrayDocs
  pure $ joinWith "\n" arrayString

{- convert do notation to bind -}
catWithBind :: Array Doc -> Doc
catWithBind xs = sequence xs >>= pure <<< joinWith "\n"

render :: Doc -> String
render doc = runReader doc 0

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \n -> do
  tell $ Additive n

collatz :: Int -> Tuple Int (Array Int)
collatz n = runWriter $ collatz' n 0 where
  collatz' :: Int -> Int -> Writer (Array Int) Int
  collatz' 1 iter = do
    tell [1]
    pure iter
  collatz' i iter = do
    tell [i]
    if i `mod` 2 == 0
      then collatz' (i / 2) (iter + 1)
      else collatz' (3 * i + 1) (iter + 1)

safeDivide :: Int -> Int -> ExceptT String (Identity) Int
safeDivide _ 0 = throwError "Division by 0"
safeDivide num denom = pure (num / denom)

runSafeDivide :: Int -> Int -> Either String Int
runSafeDivide num denom = unwrap $ runExceptT $ safeDivide num denom

type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

string :: String -> Parser String
string s = do
  state <- get
  tell [ "The state is " <> state ]
  case stripPrefix (Pattern s) state of
     (Just parsedString) -> do
       put parsedString
       pure s
     Nothing             -> throwError ["Could not parse"]

type Doc' = (WriterT (Array String) (ReaderT Level Identity)) Unit

line' :: String -> Doc'
line' s = do
  lvl <- lift $ ask
  tell [ (power "  " lvl) <> s ]

indent' :: Doc' -> Doc'
indent' = local $ \lvl -> lvl + 1

render' :: Doc' -> String
render' doc = joinWith "\n" $ unwrap $ runReaderT (execWriterT doc) 0

split :: Parser String
split = do
  s <- get
  tell ["The state is " <> s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

{- Explain MonadState --}

runParser :: forall a. Parser a -> String -> Either Errors (Tuple (Tuple a String) Log)
runParser p = unwrap <<< runExceptT <<< runWriterT <<< runStateT p

predicateParser :: (String -> String) -> Parser String
predicateParser f = do
  s <- split
  guard $ f s == s
  pure s

upper :: Parser String
upper = predicateParser toUpper

lower :: Parser String
lower = predicateParser toLower

upperOrLower :: Parser Errors
upperOrLower = some upper <|> some lower

components :: Parser (Array Errors)
components = many upperOrLower

aParser :: Parser String
aParser = string "a"

bParser :: Parser String
bParser = string "b"

multipleAsParser :: Parser Errors
multipleAsParser = many aParser

multipleBsParser :: Parser Errors
multipleBsParser = many bParser

aOrbParser :: Parser Errors
aOrbParser = some aParser <|> some bParser

execParser :: forall a. Parser a -> String -> Either Errors (Tuple String Log)
execParser p = unwrap <<< runExceptT <<< runWriterT <<< execStateT p

parseFirstThenSecond :: forall a. Parser a -> Parser a -> Parser a
parseFirstThenSecond first second = first *> second

matchFirstMultipleAsAndThenMultipleBs :: Parser Errors
matchFirstMultipleAsAndThenMultipleBs = parseFirstThenSecond (some aParser) (many bParser)

{- How can I compose multiple Parsers in sequence? -}

{- Sequence == Applicative -}

matchMultipleAsAndBs :: Parser (Array Errors)
matchMultipleAsAndBs = many aOrbParser

type Parser' = ExceptT Errors (StateT String (Writer Log))

string' :: String -> Parser' String
string' s = do
  state <- get
  tell [ "The state is " <> state ]
  case stripPrefix (Pattern s) state of
     (Just parsedString) -> do
       put parsedString
       pure s
     Nothing             -> throwError ["Could not parse"]

aParser' :: Parser' String
aParser' = string' "a"

bParser' :: Parser' String
bParser' = string' "b"

{- aOrbParser' :: Parser' Errors
aOrbParser' = some aParser' <|> some bParser'
 no type class instance for lazy was found -}

runParser' :: forall a. Parser' a -> String -> Tuple (Tuple (Either Errors a) String) Log
runParser' p s = runWriter (runStateT (runExceptT p) s)



