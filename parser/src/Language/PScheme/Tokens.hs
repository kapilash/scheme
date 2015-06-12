{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module Language.PScheme.Tokens where

data Token = OpenSqBrace !Int !Int
           | CloseSqBrace !Int !Int
           | OpenBracket !Int !Int
           | CloseBracket !Int !Int
           | R6RSChar Char !Int !Int
           | R6RSString String !Int !Int
           | R6RSIdent String !Int !Int
           | R6RSNumber Integer
           | R6RSRational (Integer, Integer) !Int !Int
           | R6RSDouble Double !Int !Int
           | R6RSComplex (Double, Double) !Int !Int
           | VectorOpen !Int !Int
           | ByteVectorOpen !Int !Int
           | Quote !Int !Int
           | BackQuote !Int !Int
           | Comma !Int !Int
           | CommaAT !Int !Int
           | Dot !Int !Int
           | HashQuote !Int !Int
           | HashBackQuote !Int !Int
           | HashComma !Int !Int
           | HashCommaAt !Int !Int
             deriving (Typeable, Data)

                         
