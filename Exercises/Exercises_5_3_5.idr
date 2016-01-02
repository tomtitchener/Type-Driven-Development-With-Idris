module Exercises_5_3_5

import Data.Vect

--5.3.5  Exercises
--In these exercises, you will find the following prelude functions useful,
-- in addition to the functions discussed earlier in the chapter:
-- openFile, closeFile, fEOF, fGetLine, and writeFile. Use :doc to find out what each of these do.

-- Missing Control.Monad from Haskell with e.g guard, forM, other combinators.
-- Means lots of in-line "do" blocks, recursive calls.

--Write a function readToBlank : IO (List String) which reads input from the console until the user enters a blank line.
readToBlank : IO (List String)
readToBlank = do
  l <- getLine
  if (l == "")
  then
    pure []
  else
    do
      ls <- readToBlank
      pure (l::ls)

--Write a function readAndSave : IO () which reads input from the console until the user enters a blank line,
-- then reads a file name from the console, and writes the input to that file.
readAndSave : IO ()
readAndSave = do
  lines      <- readToBlank
  fileName   <- getLine
  status     <- writeFile fileName $ concat lines
  case status of
    Right _  => pure ()
    Left err => do
                  printLn (show err)
                  pure ()

--Write a function readVectFile : (filename : String) -> IO (n ** Vect n String)
-- which reads the contents of a file into a dependent pair containing a length
-- and a Vect of that length.
readVectLine : (file : File) -> IO (n : Nat ** Vect n String)
readVectLine file = do
  status <- fGetLine file
  case status of
    Left  err  => do
                    printLn (show err)
                    pure (_ ** [])
    Right line => if ("" == line)
                  then
                     pure (_ ** [])
                  else do
                     (_ ** lines) <- readVectLine file
                     pure (_ ** line :: lines)

readVectFile : (name : String) -> IO (n ** Vect n String)
readVectFile name = do
  status <- openFile name Read
  case status of
    Left err   => do
                    printLn (show err)
                    pure (_ ** [])
    Right file => do
                  resp <- readVectLine file
                  closeFile file
                  pure resp
