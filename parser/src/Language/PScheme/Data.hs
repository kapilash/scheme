module Language.PScheme.Data where

import Text.Parsec


data Definition = Definition String String
                deriving (Eq,Show)

data Module = Module String [Definition]
            deriving (Eq,Show)

newtype Ident = Ident String
    deriving (Eq,Show,Ord)

data PScheme = PInt Int
               |PDouble Double
               |PString String
               |PSymbol String
               |PPair PScheme PScheme
               |PLambda [String] PScheme
               |PLet [(String, PScheme)] PScheme
               |PIF PScheme PScheme PScheme
               |PBool Bool
               |PNil
                
