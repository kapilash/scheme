module Main where

import Language.PostFix


main :: IO ()
main = mapM_ runValidCase validCases


data ValidCase = ValidCase String [Int] Int

instance Show ValidCase where
  show (ValidCase s i v) = s ++ (show i) ++ " (expecting " ++ (show v) ++ ")"

runValidCase :: ValidCase -> IO ()
runValidCase v@(ValidCase s args i) = do
  putStr (show v)
  putStr " ---> "
  case runStr s args of
    Right (PFNumber r) -> if i == r
                          then putStrLn "Success"
                          else putStrLn $ "FAIL. Expected " ++ (show i) ++ " got " ++ (show r)
    e                  -> putStrLn $ "Failed with " ++ (show e)

runStr :: String -> [Int] -> Either PFError PFElem
runStr str args = case parseStr str of
                    Left err -> Left InvalidProg
                    Right p -> runProgram p args


validCases :: [ValidCase]
validCases = [
   ValidCase "(postfix 2)" [3,4] 3,
   ValidCase "(postfix 2 swap)" [3,4] 4,
   ValidCase "(postfix 3 pop swap)" [3,4,5] 5,
   ValidCase "(postfix 1 4 sub)" [3] (-1),
   ValidCase "(postfix 1 4 add 5 mul 6 sub 7 div)" [3] 4,
   ValidCase "(postfix 5 add mul sub swap div)" [7,6,5,4,3] (-20),
   ValidCase "(postfix 3 4000 swap pop add)" [300,20,1] 4020,
   ValidCase "(postfix 2 add 2 div)" [3,7] 5,
   ValidCase "(postfix 1 3 div)" [17] 5,
   ValidCase "(postfix 1 3 rem)" [17] 2,
   ValidCase "(postfix 1 4 lt)" [3] 1,
   ValidCase "(postfix 1 4 lt)" [5] 0,
   ValidCase "(postfix 1 4 lt 10 add)" [3] 11,
   ValidCase "(postfix 2 1 nget)" [4,5] 4,
   ValidCase "(postfix 2 2 nget)" [4,5] 5,
   ValidCase "(postfix 1 1 nget mul)" [5] 25,
   ValidCase "(postfix 4 4 nget 5 nget mul mul swap 4 nget mul add add)" [3,4,5,2] 25,
   ValidCase "(postfix 1 (2 mul) exec)" [7] 14,
   ValidCase "(postfix 0 (0 swap sub) 7 swap exec)" [] (-7),
   ValidCase "(postfix 1 2 3 sel)" [1] 2,
   ValidCase "(postfix 1 2 3 sel)" [0] 3,
   ValidCase "(postfix 1 2 3 sel)" [17] 2,
   ValidCase "(postfix 4 lt (add) (mul) sel exec)" [3,4,5,6] 30,
   ValidCase "(postfix 4 lt (add) (mul) sel exec)" [4,3,5,6] 11,
   ValidCase "(postfix 1 1 nget 0 lt (0 swap sub) () sel exec)" [-7] 7,
   ValidCase "(postfix 1 1 nget 0 lt (0 swap sub) () sel exec)" [6] 6
  ]
