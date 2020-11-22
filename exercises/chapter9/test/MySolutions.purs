module Test.MySolutions where

import Prelude

import Data.Either (Either(..))
import Data.String.CodeUnits (length)
import Data.Traversable (traverse)
import Effect.Aff (Aff, attempt, message)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)

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




