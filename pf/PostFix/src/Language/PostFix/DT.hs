module Language.PostFix.DT where


import qualified Data.Map as Map
import Control.Monad


data PFError = PFInvalidArgCount Int Int
             | PFInvalidArg String
             | PFNonNumber String
             deriving Show

data PFElem = PFNumber Integer
            | PFCmdName String
            | PFSequence [PFElem]
            deriving Show

newtype PFCommand a = PFCmd {prunCmd :: PFEnv -> Either PFError (a, PFEnv) }

data PFEnv = PFEnv { pfStack :: [PFElem],
                     pfCmds :: [PFElem]
                   }

mapPFCmd :: (a -> b) -> PFCommand a -> PFCommand b
mapPFCmd f (PFCmd cmd) = PFCmd $ \env ->
                case cmd env of
                  Left err -> Left err
                  Right (av, e') -> Right (f av, e')

instance Functor PFCommand where
  fmap = mapPFCmd


retCmd :: a -> PFCommand a
retCmd aval = PFCmd $ \env -> Right (aval, env)

bindCmd :: PFCommand a -> (a -> PFCommand b) -> PFCommand b
bindCmd (PFCmd cmda) f = PFCmd $ \env ->
  case cmda env of
    Left err  -> Left err
    Right (av, e') -> prunCmd (f av) $  e'
                        

data PFProgram = PFProgram [PFElem] Int

runProgram :: PFProgram -> [Int] -> Either PFError PFElem
runProgram (PFProgram commands argCount) list
  | length list /= argCount  = Left $ PFInvalidArgCount argCount (length list)
  | otherwise                = error "interpreter not implemented"
