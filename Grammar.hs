module Grammar where

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Lexeme

data Type = Int | Double | String deriving Show

data Var = Var { varType :: Type, varName :: String } deriving Show

buildInTypes :: Map.Map String Type
buildInTypes = Map.fromList [("int", Int), ("double", Double), ("string", String)]

data UnaryOperation = Not | Neg deriving Show

unaryOperations :: Map.Map String UnaryOperation
unaryOperations = Map.fromList [("not", Not), ("-", Neg)]

data BinaryOperation = And | Or | Eq | B | L | BE | LE | NotE | Sum | Sub | Mul | Div deriving Show

binaryOperations :: Map.Map String BinaryOperation
binaryOperations = Map.fromList [("and", And), ("or", Or),("==", Eq),
				 (">", B), ("<", L), (">=", BE),
				 ("<=", LE), ("!=", NotE),("+", Sum),
				 ("-", Sub), ("*", Mul), ("/", Div)]

data Func = Func { returnType :: Type
                 , funcName :: String
                 , args :: [Var]
                 , statementList :: [Stmt]
                 } deriving Show

data FuncCall = FuncCall String [Expr] deriving Show

data Expr = IntLit Int
                | DblLit Double
                | StrLit String
                | UnaryExpr UnaryOperation Expr
                | BinaryExpr BinaryOperation Expr Expr
                | FCall FuncCall
                | VCall String
deriving Show

data Stmt = VarDef Var Expr
               | VarAssign String Expr
               | FuncDef Func
               | If Expr [Stmt] [Stmt]
               | While Expr [Stmt] 
               | Return (Maybe Expr)
               | FCall FuncCall
deriving Show

type ASTree = [Func]

type FuncBody = [Stmt]

	languageDef =
  emptyDef { Lexeme.identStart      = letter
           , Lexeme.identLetter     = alphaNum
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
