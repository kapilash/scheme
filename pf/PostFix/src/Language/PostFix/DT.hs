module Language.PostFix.DT where


import qualified Data.Map as Map
import Control.Monad
import Control.Applicative

data PFError = PFInvalidArgCount Int Int
             | PFInvalidArg String
             | PFNonNumber
             | PFStackEmpty
             deriving Show

data PFElem = PFNumber Integer
            | PFCmdName String
            | PFSequence [PFElem]
            deriving Show

newtype PFCommand a = PFCmd {prunCmd :: PFEnv -> Either PFError (a, PFEnv) }

data PFEnv = PFEnv { pfStack :: [PFElem],
                     pfCmds :: [PFElem],
                     pfDefs :: Map.Map String (PFCommand ())
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


seqCmd :: PFCommand (a -> b) -> PFCommand a -> PFCommand b
seqCmd (PFCmd aToBRun) (PFCmd aRun) = PFCmd $ \env ->
  case aToBRun env of
    Left err  -> Left err
    Right (aTob, env') -> case aRun env of
      Left err -> Left err
      Right (aval, env'') -> Right (aTob aval, env'')


instance Applicative PFCommand where
  pure  = retCmd
  (<*>) =  seqCmd

bindCmd :: PFCommand a -> (a -> PFCommand b) -> PFCommand b
bindCmd (PFCmd cmda) f = PFCmd $ \env ->
  case cmda env of
    Left err  -> Left err
    Right (av, e') -> prunCmd (f av) $  e'
                        
instance Monad PFCommand where
  return = retCmd
  (>>=) = bindCmd

  
data PFProgram = PFProgram [PFElem] Int

runProgram :: PFProgram -> [Int] -> Either PFError PFElem
runProgram (PFProgram commands argCount) list
  | length list /= argCount  = Left $ PFInvalidArgCount argCount (length list)
  | otherwise                = error "interpreter not implemented"


writeNum :: Integer -> PFCommand ()
writeNum i = PFCmd $ \env ->
  case env of
    PFEnv stack cmd defs  -> Right ((), PFEnv ((PFNumber i):stack) cmd defs)
  

readNum :: PFCommand Integer
readNum = PFCmd $ \env ->
  case pfStack env of
    (PFNumber i):rest -> Right (i, PFEnv rest (pfCmds env) (pfDefs env))
    []                -> Left PFStackEmpty
    _                 -> Left PFNonNumber


subtract :: PFCommand ()
subtract = do
  i1 <- readNum
  i2 <- readNum
  writeNum (i2 - i1)

