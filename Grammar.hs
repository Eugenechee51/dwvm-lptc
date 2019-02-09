module Grammar where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Lexeme

data Variable = IntVar Int | DblVar Double | StrVar String deriving Show

	languageDef =
  emptyDef { Lexeme.identStart      = letter
           , Lexeme.identLetter     = alphaNum {What is it?}
           , Lexeme.reservedNames   = [ 
					"int"
				      , "double"
				      , "string"
			      	      , "if"
                               	      , "else"
                               	      , "while"
                               	      , "return"
                               	      , "print"
                       	 	      , "void"
                                      ]
            	Lexeme.reservedOpNames = ["+", "-", "=", "*", "/",
                                       "<", ">", ">=", "<=", "and",
                                       "or", "not"
                                      ]
            }
