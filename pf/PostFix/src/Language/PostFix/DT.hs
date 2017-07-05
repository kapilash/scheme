module Language.PostFix.DT where


import qualified Data.Map as Map
import Control.Monad
import Control.Applicative

data PFError = PFInvalidArgCount Int Int
             | PFInvalidOper String
             | PFNonNumber
             | PFStackEmpty
             deriving Show

data PFElem = PFNumber Int
            | PFCmdName String
            | PFSeq [PFElem]

instance Show PFElem  where
  show (PFNumber i) = show i
  show (PFCmdName s) = s
  show (PFSeq e) = show e

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

failCmd :: String -> PFCommand a
failCmd msg = PFCmd $ \env -> Left $ PFInvalidOper msg
                      
instance Monad PFCommand where
  return = retCmd
  (>>=) = bindCmd
  fail = failCmd

  
data PFProgram = PFProgram [PFElem] Int

runProgram :: PFProgram -> [Int] -> Either PFError PFElem
runProgram (PFProgram commands argCount) list
  | length list /= argCount  = Left $ PFInvalidArgCount argCount (length list)
  | otherwise                = interpreter (PFEnv (map PFNumber list) commands (Map.fromList cmdMapping))


writeNum :: Int -> PFCommand ()
writeNum i = PFCmd $ \env ->
  case env of
    PFEnv stack cmd defs  -> Right ((), PFEnv ((PFNumber i):stack) cmd defs)
  

readNum :: PFCommand Int
readNum = PFCmd $ \(PFEnv stack cmds defs) ->
  case stack of
    (PFNumber i):rest -> Right (i, PFEnv rest cmds defs)
    []                -> Left PFStackEmpty
    _                 -> Left PFNonNumber

nextElem :: PFCommand PFElem
nextElem = PFCmd $ \(PFEnv stack cmds defs) ->
  case stack of
    x:rest -> Right (x, PFEnv rest cmds defs)
    _      -> Left PFStackEmpty

readNthElem :: Int -> PFCommand PFElem
readNthElem num
  | num >= 0 = PFCmd $ \(PFEnv stack cmds defs) ->
      if length stack > num then Right (stack !! num, PFEnv stack cmds defs)
      else Left PFStackEmpty
  | otherwise = PFCmd $ \x -> Left . PFInvalidOper $ "readNthElem " ++ (show num)
    

pushElem :: PFElem -> PFCommand ()
pushElem elem = PFCmd $ \(PFEnv stack cmds defs) ->
  Right ((), PFEnv (elem : stack) cmds defs)

pushCmd :: PFElem -> PFCommand ()
pushCmd cmd = PFCmd $ \(PFEnv stack cmds defs) ->
  Right ((), PFEnv stack (cmd:cmds) defs)


arithOper :: (Int -> Int -> Int) -> PFCommand()
arithOper oper = do
  i1 <- readNum
  i2 <- readNum
  writeNum $ oper i2 i1



cmpOper :: (Int-> Int-> Bool) -> PFCommand()
cmpOper oper = do
  i1 <- readNum
  i2 <- readNum
  let v = if oper i2 i1
          then 1
          else 0
  writeNum v


divide :: PFCommand ()
divide = do
  i1 <- readNum
  i2 <- readNum
  if i1 == 0
    then fail "divide by zero"
    else writeNum $ i2 `div` i1
    
remainder :: PFCommand ()
remainder = do
  i1 <- readNum
  i2 <- readNum
  if i1 == 0
    then fail "modulo zero"
    else writeNum $ i2 `rem` i1


pop :: PFCommand ()
pop = do
  nextElem
  return ()


swap :: PFCommand ()
swap = do
  p1 <- nextElem
  p2 <- nextElem
  pushElem p1
  pushElem p2

sel :: PFCommand()
sel = do
  v1 <- nextElem
  v2 <- nextElem
  v3 <- readNum
  if v3 == 0
    then pushElem v1
    else pushElem v2

nget :: PFCommand ()
nget = do
  vi <- readNum
  nth <- readNthElem (vi - 1)
  pushElem nth



exec :: PFCommand ()
exec = do
  n <- nextElem
  case n of
    PFSeq s -> mapM_ pushCmd s
    _       -> fail "Not sequence"

cmdMapping = [
  ("exec", exec),
  ("nget", nget),
  ("sel", sel),
  ("swap", swap),
  ("pop", pop),
  ("sub", arithOper (-)),
  ("add", arithOper (+)),
  ("mul", arithOper (*)),
  ("div", divide),
  ("rem", remainder),
  ("lt", cmpOper (<)),
  ("gt", cmpOper (>)),
  ("eq", cmpOper (==))
  ]

  

interpreter :: PFEnv -> Either PFError PFElem
interpreter (PFEnv [] _ _) =  Left PFStackEmpty
interpreter (PFEnv stack [] _) = Right $ head stack
interpreter (PFEnv stack (e:elems) defs) =
  case e of                                     
    PFCmdName s -> case Map.lookup s defs of
                     Nothing -> Left $ PFInvalidOper s
                     Just (PFCmd c) -> case c (PFEnv stack elems defs) of
                                         Left err -> Left err
                                         Right (_,env') -> interpreter env'
    otherwise -> interpreter (PFEnv (e:stack) elems defs)
