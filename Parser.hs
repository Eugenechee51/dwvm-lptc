module Parser where

import Grammar
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Lexeme

lexer         = Lexeme.makeTokenParser languageDef    --lexical analyzer

identifier    = Lexeme.identifier    lexer -- parses an identifier
reserved      = Lexeme.reserved      lexer -- parses a reserved name
reservedOp    = Lexeme.reservedOp    lexer -- parses an operator
parens        = Lexeme.parens        lexer 
braces        = Lexeme.braces        lexer
integer       = Lexeme.integer       lexer -- parses an integer
float         = Lexeme.float         lexer -- parses a float
stringLiteral = Lexeme.stringLiteral lexer 
stringLiteral = Lexeme.stringLiteral lexer
whiteSpace    = Lexeme.whiteSpace    lexer

int :: Parser Int
int = fromInteger <$> Lexeme.integer lexer

tabsSpaces :: Parser ()
tabsSpaces = skipMany $ oneOf "\t "
varDefine :: Parser String
varDefine = do
   spaces
   id <- identifier
   spaces
   char '='
   spaces
   return id

