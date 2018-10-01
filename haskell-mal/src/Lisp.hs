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

identifier :: Parsec SExpression
identifier = Identifier <$> pIdentifier
           where pIndentifier = (:) <$> letter <*> many (alphaNum <|> char '-')

atom :: Parsec SExpression
atom = Atom . Number . read <$> many1 digit
   <|> Atom . String . read <$> (char '"' *> many anychar <* char '"')
   <|> Atom . Char . read <$> (char '\'' *> anychar <* char '\'')

parseLine = parse atom ""
