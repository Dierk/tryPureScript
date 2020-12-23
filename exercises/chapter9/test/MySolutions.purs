module Test.MySolutions where

import Data.Either
import Data.Maybe
import Effect.Aff
import Prelude

import Affjax (Response)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parOneOf, parTraverse)
import Data.String (Pattern(..), length, split)
import Data.Traversable (fold, traverse)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Node.Path as Path
import Test.HTTP (getUrl)
import Data.Array((:), concat)

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles src1 src2 dest = do
  str1 <- readTextFile UTF8 src1
  str2 <- readTextFile UTF8 src2
  writeTextFile UTF8 dest $ str1 <> str2

-- there is no : operator for Arrays due to performance problems!
concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany [] _    = pure unit
concatenateMany xs dest = do
 strs <- traverse (readTextFile UTF8) xs
 writeTextFile UTF8 dest $ fold strs

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters src =
  attempt do
    str <- readTextFile UTF8 src
    pure $ length str

writeGet :: String -> FilePath -> Aff Unit
writeGet url dest = do
  eitherResponse <- AX.get ResponseFormat.string url
  case eitherResponse of
    Left err       -> log $ AX.printError err
    Right response -> writeTextFile UTF8 dest response.body

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel [] _ = pure unit
concatenateManyParallel xs dest = do
  strs <- parTraverse (readTextFile UTF8) xs
  writeTextFile UTF8 dest $ fold strs

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout limit url =
  parOneOf
    [ Just    <$> getUrl url
    , Nothing <$ (delay $ Milliseconds limit)
    ]

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles src = do
  str <- readTextFile UTF8 src
  case str of
    "" -> pure [ src ]
    c -> do
      let
        dir = Path.dirname src
        files = split (Pattern "\n") str
        filesFromRoot = map (\f -> Path.concat [ dir, f ]) files
      xs <- parTraverse recurseFiles filesFromRoot
      pure $ src : concat xs