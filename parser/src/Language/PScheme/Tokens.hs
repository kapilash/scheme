{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module Language.PScheme.Tokens where

data Token = OpenSqBrace !Int !Int
           | CloseSqBrace !Int !Int
           | OpenBracket !Int !Int
           | CloseBracket !Int !Int
           | PChar Char !Int !Int
           | PString String !Int !Int
           | PIdent String !Int !Int
           | PNumber Integer
           | BackQuote !Int !Int
           | Comma !Int !Int
           | CommaAT !Int !Int
           | Dot !Int !Int
           | DollarId String !Int !Int
           | DefMacro !Int !Int
           | DefStruct !Int !Int
           | DefFunc !Int !Int
           | DefProc !Int !Int
           | OpenBrace !Int !Int
           | CloseBrace !Int !Int
           | PLet !Int !Int
           | PLambda !Int !Int
           | PIf !Int !Int
           | PElse !Int !Int
           | PCond !Int !Int
           | PNil !Int !Int
           | PTrue !Int !Int
             deriving (Typeable, Data)

                         
