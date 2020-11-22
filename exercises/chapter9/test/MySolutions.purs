module Test.MySolutions where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parTraverse, parOneOf)
import Data.Array (concat, elem, filter, foldM)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (length)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, message, Milliseconds(..), delay, runAff_)
import Effect.Class.Console (log, logShow)
import Effect.Exception (Error, throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath, dirname, sep)


{-
  This is so typical: once we run in the browser, the code is no longer tested :-(((

  Dependency on npm install because of xhr2 is __really__ bad as it 
  totally undermines the potentially much improved purescript story for dependency management.

  Async/await is really just syntax sugar around Promises and to some degree a disguise.
  NB: await is like `<-` and async is like `do` in a monadic context.    

  "Callbacks lead to excessive nesting" is not true for the monadic "then" composition.

  Some additional installations were needed:
  spago install node-fs-aff node-path aff

-} 

copyFile :: FilePath -> FilePath -> Aff Unit
copyFile file1 file2 = do
  my_data <- readTextFile UTF8 file1
  writeTextFile UTF8 file2 my_data

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles file1 file2 resultFile= do
  my_data1 <- readTextFile UTF8 file1
  my_data2 <- readTextFile UTF8 file2
  writeTextFile UTF8 resultFile (my_data1 <> my_data2) -- well, appending would be more efficient
 
{-
traverse :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)

traverse :: (FilePath -> Aff Unit) -> Array FilePath -> Aff (Array Unit)
-} 
concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany files resultFile = do
  writeTextFile UTF8 resultFile ""  -- make sure the file exists and is empty
  _ <- traverse (\file -> concatenateFiles file resultFile resultFile) files 
  pure unit

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters file =
  map (map length) (attempt $ readTextFile UTF8 file)

{-
    Notes: Aff monadic bind ensures sequence

    "parallel" != "concurrent"                 
-}

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel files resultFile = do
    contents <- parTraverse (\file -> readTextFile UTF8 file) files
    writeTextFile UTF8 resultFile (foldr (<>) "" contents)
    pure unit

-- copied from book
getUrl :: String -> Aff String
getUrl url = do
  result <- AX.get ResponseFormat.string url
  pure $ case result of
    Left err -> "GET /api response failed to decode: " <> AX.printError err
    Right response -> response.body   
-- end of copy

{-
    At this point one has to scan through pursuit for possible candidate functions.

    parOneOf :: forall a t m f. Parallel f m => Alternative f => Foldable t => Functor t => t (m a) -> m a

    parOneOf :: forall f. Parallel f Aff => Alternative f => Array (Aff (Maybe String)) -> Aff (Maybe String)
-}

myDelay :: Number -> Aff (Maybe String)
myDelay ms = do
  _ <- delay (Milliseconds ms)
  pure Nothing

myGetUrl :: String -> Aff (Maybe String)  
myGetUrl url = do
  content <- getUrl url
  pure $ Just content

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout ms url = 
  parOneOf [ myDelay ms, myGetUrl url ]

-- well, we cannot read files without being at least an Effect (or Aff)

lineContent :: FilePath -> Aff (Array String)
lineContent file = do
  -- log file
  contents <- readTextFile UTF8 file
  pure $ filter (_ /= "") (lines contents)

combine :: Array FilePath -> FilePath -> Aff (Array FilePath)
combine filesAcc file = 
    if file `elem` filesAcc -- guard against endless recursion
    then 
        pure filesAcc 
    else do
        lines    <- lineContent file
        recursed <- recurseFiles $ map (\line -> dirname file <> sep <> line) lines 
        pure (filesAcc <> [file] <> recursed) 

recurseFiles :: (Array FilePath) -> Aff (Array FilePath)
recurseFiles files = 
  map concat $ parTraverse (\file -> foldM combine [] [file]) files
   