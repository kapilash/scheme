module Main where

import Language.PostFix
import System.Environment(getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    file:rest -> do 
      parsed <- parsePFile file
      case parsed of
        Left err -> do { print err; return () }
        Right prog -> case runProgram prog (map read rest) of
                        Left err -> print err
                        Right v  -> print v
    _  -> putStrLn "need a file and some arguments"
