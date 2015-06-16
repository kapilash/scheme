module Language.PScheme.Data where


import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Except       
import qualified Data.Map as Map
import qualified Data.List as Lst


data Definition = Definition String String
                deriving (Eq,Show)

data Module = Module String [Definition]
            deriving (Eq,Show)


     
data PScheme = PInt Int
               |PDouble Double
               |PString String
               |PSymbol String
               |PVar String
               |PPair PScheme PScheme
               |PLambda [(String,PScheme)] PScheme
               |PLet [(String, PScheme)] PScheme
               |PCond [(PScheme, PScheme)]
               |PBool Bool
               |PNil
               |PError String

instance Show PScheme where
         show (PInt i)   = show i
         show (PDouble d) = show d
         show (PString s) = "\"" ++ s ++ "\""
         show (PSymbol s) = "@" ++ s
         show (PVar s)  = s
         show (PPair f r) = "(" ++ (show f) ++ " " ++ (show r) ++ ")"
         show (PLambda _ _) = "<lambda>"
         show (PLet _ _)   = "<let-expr>"
         show (PCond _ )   = "<condition>"
         show (PBool b)    = if b then "#T" else "#F"
         show PNil         = "Nil"
         show (PError str) = "Error : (" ++ str ++ ")"

         
data PMacro = PMacro String [String] [(PScheme, PScheme)]
            deriving (Show)


     
newtype Stack = Stack [Map.Map String PScheme]

setVal (Stack (h:rest)) str v  = Stack ((Map.insert str v h):rest)

getVal (Stack stack) str = case Lst.dropWhile (Map.notMember str) stack of
                             [] -> (PError "Undefined", Stack stack)
                             (h:_) -> (h Map.! str, Stack stack)

pushEmpty  (Stack stack) = Stack $ (Map.empty) : stack




type PState = State [Map.Map String PScheme]

type PExecState = ExceptT String (State [Map.Map String PScheme])

type PSchemeEnv = ContT PScheme PExecState PScheme     
