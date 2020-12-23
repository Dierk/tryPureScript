module Split where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Writer (runWriter)
import Control.Monad.Except.Trans (ExceptT, throwError, runExceptT)
import Control.Monad.State.Trans (StateT, runStateT, get, put)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Control.MonadPlus (guard)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.String (take, drop, toUpper, toLower)
import Data.Tuple (Tuple)
import Data.Array
import Data.String.CodeUnits (stripPrefix, toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Maybe (Maybe(..))
import Control.Alternative
import Control.Alt

type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split :: Parser String
split = do
  s <- get
  tell ["The state is " <> show s]
  case s of
    "" -> throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)

eof :: Parser Unit
eof = do
  s <- get
  tell ["The state is " <> show s]
  case s of
    "" -> pure unit
    _ -> throwError ["Expected end-of-file"]

upper :: Parser String
upper = do
  s <- split
  guard $ toUpper s == s
  pure s

lower :: Parser String
lower = do
  s <- split
  guard $ toLower s == s
  pure s

runParser :: forall a. Parser a -> String -> Either Errors (Tuple (Tuple a String) Log)
-- runExcept :: ∀ Errors a. Except Errors a → Either Errors a
-- runWriterT :: ∀ w m a. WriterT w m a → m (Tuple a w)
-- runStateT :: ∀ s m a. StateT s m a → s → m (Tuple a s)
runParser p = runExcept <<< runWriterT <<< runStateT p

type Parser' = ExceptT Errors (StateT String (WriterT Log Identity))

runParser' :: forall a. Parser' a -> String -> Tuple (Tuple (Either Errors a) String) Log
-- runWriter :: ∀ w a. Writer w a → Tuple a w ;  Tuple (Either (Array String) a0)
-- runStateT :: ∀ s m a. StateT s m a → s → m (Tuple a s) ; WriterT (Array String)
-- runExceptT :: ∀ e m a. ExceptT e m a → m (Either e a) ; StateT String
runParser' p s = runWriter (runStateT (runExceptT p) s)

--runParser (string "asdf") "asdfewrwer"
string :: String -> Parser String
string prefix = do
  s <- get
  tell ["The state is " <> s]
  case stripPrefix (Pattern prefix) s of
    Nothing -> throwError ["Could not parse"]
    (Just suffix) -> do 
      put suffix
      pure prefix

--runParser aOrb "bbaabb"
aOrb :: Parser (Array String)
aOrb = some (string "a") <|> some (string "b")

--runParser components  "bbaabb"
components :: Parser (Array (Array String))
components = many aOrb
