module Language.PostFix.DT where


import qualified Data.Map as Map

data PFError = PFInvalidArgCount Int Int
             | PFInvalidArg String
             | PFNonNumber String
             deriving Show

data PFElem = PFNumber Integer
            | PFCmdName String
            | PFSequence [PFElem]
            deriving Show

newtype PFCommand = PFCmd (PFEnv -> Either PFError PFEnv)

data PFEnv = PFEnv [PFElem] [PFElem] (Map.Map String PFCommand)


data PFProgram = PFProgram [PFElem] Int

runProgram :: PFProgram -> [Int] -> Either PFError PFElem
runProgram (PFProgram commands argCount) list
  | length list /= argCount  = Left $ PFInvalidArgCount argCount (length list)
  | otherwise                = error "not implemented"
