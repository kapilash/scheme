module Language.PostFix.Parser where

import Language.PostFix.DT
import Text.Parsec
import Text.Parsec.String

parsePFile :: FilePath -> IO (Either ParseError PFProgram)
parsePFile fileName = parseFromFile pfProg fileName

parseStr :: String -> Either ParseError PFProgram
parseStr s = parse pfProg "instring" s

parseCmdStr :: Parser String
parseCmdStr = try (string "sub")
              <|> try (string "add")
              <|> try (string "mul")
              <|> try (string "div")
              <|> try (string "rem")
              <|> try (string "lt")
              <|> try (string "gt")
              <|> try (string "eq")
              <|> try (string "pop")
              <|> try (string "sel")
              <|> try (string "swap")
              <|> try (string "nget")
              <|> try (string "exec")
              <?> "invalid command"

parseCmd :: Parser PFElem
parseCmd = do
  s <- parseCmdStr
  return $ PFCmdName s

numeric :: Parser Int
numeric = do
  n <- many1 digit
  return $ read n

parseNum :: Parser PFElem
parseNum = do
  n <- numeric
  return $ PFNumber n

parseSeq :: Parser PFElem
parseSeq = do
  char '('
  elems <- sepBy pfToken ignorable
  char ')'
  return $ PFSeq elems
  
pfToken :: Parser PFElem
pfToken = parseCmd <|> parseSeq <|> parseNum

lineComment :: Parser ()
lineComment = do
  char '#'
  manyTill anyChar (try newline)
  return ()

ignorable1 :: Parser ()
ignorable1 = lineComment <|> (space >> return ())

ignorable :: Parser ()
ignorable = do
  many ignorable1
  return ()

pfProg :: Parser PFProgram
pfProg = do
  ignorable
  char '('
  ignorable
  string "postfix"
  ignorable
  argCount <- numeric
  ignorable
  elems <- endBy pfToken ignorable
  char ')'
  ignorable
  return $ PFProgram elems argCount
  
