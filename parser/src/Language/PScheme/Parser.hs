module Language.PScheme.Parser where

import Text.ParserCombinators.Parsec
import Language.PScheme.Data
import qualified Text.Parsec.Token as P    
import Text.Parsec.Language
import Data.Char(isSpace)

eol' = try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "End of Line"

eol = do
  many1 eol'
  return ()

lineComment = do
  optional (char '#')
  char ';'
  many $ satisfy (\x -> x /= '\r' && x /= '\n')
  (eol <|> eof)


blockComment = do
  string "#|"
  manyTill ((try blockComment) <|> (anyChar >> return ())) (try $ string "|#")
  return ()

gap = many1 gap' 
      where gap' = skipMany1 space
                   <|> try lineComment
                   <|> blockComment
                   <|> eol
                   <?> "Comment or whitespace"
