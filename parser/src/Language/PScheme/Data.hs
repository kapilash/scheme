module Language.PScheme.Data where

data Definition = Definition String String
                deriving (Eq,Show)

data Module = Module String [Definition]
            deriving (Eq,Show)
