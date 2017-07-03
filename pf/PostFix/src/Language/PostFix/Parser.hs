module Language.PostFix.Parser where

import Language.PostFix.DT
import Text.Parsec

parsePFile :: FilePath -> IO (Either String PFProgram)
parsePFile fileName = return $ Left "Not implemented"
