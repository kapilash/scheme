module Language.Cil.Types where

import Data.Int
import Data.Word


data CilChar = CilSChar Char
             | CilEscaped Char
             | CilOctal Int
               deriving Show

data Constant = CilInt32 Int32
              | CilInt64 Int64
              | CilReal Double
              | CilHexByte Word8
              | CilQString [CilChar]
              | CilSQString [CilChar]
                deriving Show

newtype CilID = CilID String
    deriving (Eq,Show,Ord)

data CilDottedName = CilDottedName [String]
                     deriving (Eq, Show, Ord)

type CilLabel = CilID                              
