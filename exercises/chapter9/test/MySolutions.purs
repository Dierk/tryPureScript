module Test.MySolutions where

import Prelude
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Data.Traversable (fold, traverse)
import Node.Path (FilePath)
import Node.Path as Path
import Data.String (Pattern(..), length, split)
import Test.HTTP
import Control.Parallel (parTraverse, parOneOf)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Array (concat, (:))

-- Note to reader: Add your solutions to this file
concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles file1 file2 outFile = do
  my_data1 <- readTextFile UTF8 file1
  my_data2 <- readTextFile UTF8 file2
  writeTextFile UTF8 outFile (my_data1 <> my_data2)


concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany arr outFile = do
  res <- traverse (\file -> readTextFile UTF8 file) arr
  writeTextFile UTF8 outFile (fold res)

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters file = attempt do
  contents <- readTextFile UTF8 file
  pure $ length contents


writeGet :: String -> FilePath -> Aff Unit
writeGet url outFile = do
  my_data <- getUrl url
  writeTextFile UTF8 outFile my_data

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel arr outFile = do
  res <- parTraverse (\file -> readTextFile UTF8 file) arr
  writeTextFile UTF8 outFile (fold res)

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout ms url = 
  parOneOf 
    [ AX.get ResponseFormat.string url <#> hush <#> map _.body
    , delay (Milliseconds ms) $> Nothing
    ]

recurseFiles :: FilePath -> Aff (Array String)
recurseFiles file = do
  contents <- readTextFile UTF8 file
  case contents of
    "" -> pure [file]
    c -> do
      let
        dir = Path.dirname file

        files = split (Pattern "\n") contents

        filesFromRoot = map (\f -> Path.concat [ dir, f ]) files
      arrarr <- parTraverse recurseFiles filesFromRoot
      pure $ file : concat arrarr