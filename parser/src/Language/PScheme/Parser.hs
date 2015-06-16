module Language.PScheme.Parser where

import Text.Parsec
import Language.PScheme.Data
import Data.Char(isSpace)
import Text.Parsec.String as P
import Text.Parsec.Language (emptyDef,haskellDef)
import qualified Text.Parsec.Token  as PT
import Data.Functor.Identity
       
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

gap' = skipMany1 space
       <|> try lineComment
       <|> blockComment
       <|> eol
       <?> "Comment or whitespace"

{-lexeme p = do
  x <- p
  pos <- getPosition
  many1 gap'
  return (x,pos)-}

begin ::  P.Parser (Char,SourcePos)
begin = do
  c <- oneOf "[({"
  pos <- getPosition
  many1 gap'
  return (c,pos)

matchingClose '(' = ')'
matchingClose '[' =  ']'
matchingClose '{' = '}'
matchingClose  _  = error "internal error for matchingClose"
                    
end :: Char -> P.Parser SourcePos
end c = do
   char (matchingClose c)
   pos <- getPosition
   many1 gap'
   return pos

wrapped p = do
    (c,pos1) <- begin
    res <- p
    pos2 <- end c
    return (pos1, res, pos2)


pscheme :: PT.GenLanguageDef String u Identity
pscheme = emptyDef {
        PT.reservedNames = ["let","cond","def-macro","if","def-struct","define","lambda","syntax","def-module"],
        PT.commentStart = "/*",
        PT.commentEnd = "*/",
        PT.commentLine = ";",
        PT.nestedComments = True,
        PT.identStart = upper <|> oneOf "!$%&*+./<=>?@\\|-~",
        PT.identLetter    = alphaNum <|> oneOf "!$%&*+./<=>?@\\|-~",
        PT.opStart = char ':',
        PT.opLetter  = oneOf ":!$%&*+./<=>?@\\|-~",
        PT.reservedOpNames = [".","^","#\\"]   
        }

        
lexer = PT.makeTokenParser pscheme

parens = PT.parens lexer
braces = PT.braces lexer
identifier = (PT.identifier  lexer) <|> PT.operator lexer

