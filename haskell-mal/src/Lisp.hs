module Lisp where

import Text.Parsec

data Keyword = If | Lambda
     deriving ( Show )

data Atom = String String
          | Char Char
          | Number Int
     deriving ( Show )

data SExpression = List [SExpression]
                 | Identifier String
                 | Keyword Keyword
                 | Atom Atom
     deriving ( Show )

identifier :: Parsec String () SExpression
identifier = Identifier <$> pIdentifier
           where pIdentifier = (:) <$> letter <*> many (alphaNum <|> char '-')

atom :: Parsec String () SExpression
atom = Atom . Number . read <$> many1 digit
   <|> Atom . String <$> (char '"' *> many (noneOf "\"") <* char '"')
   <|> Atom . Char <$> (char '\'' *> anyChar <* char '\'')

parseLine = parse atom "REPL"
