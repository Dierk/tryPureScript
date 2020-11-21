module Test.MySolutions where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff, attempt, message)
import Effect.Class.Console (log)
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
 

