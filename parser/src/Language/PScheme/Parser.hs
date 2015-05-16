module Language.PScheme.Parser where

import Text.ParserCombinators.Parsec
import Language.PScheme.Data

eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "End of Line"


    
