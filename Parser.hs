{-# LANGUAGE FlexibleContexts #-}
module Parser (asTree) where

import Grammar
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.Char
import qualified Data.Map as Map
import qualified Text.ParserCombinators.Parsec.Token as Lexeme

lexer         = Lexeme.makeTokenParser languageDef    --lexical analyzer

identifier    = Lexeme.identifier    lexer -- parses an identifier
reserved      = Lexeme.reserved      lexer -- parses a reserved name
reservedOp    = Lexeme.reservedOp    lexer -- parses an operator
parens        = Lexeme.parens        lexer 
braces        = Lexeme.braces        lexer
semi          = Lexeme.semi          lexer
commaSep      = Lexeme.commaSep      lexer
integer       = Lexeme.integer       lexer -- parses an integer
float         = Lexeme.float         lexer -- parses a float
stringLiteral = Lexeme.stringLiteral lexer
wSpace        = Lexeme.whiteSpace    lexer

getKey :: Eq b => b -> Map.Map c b -> c
getKey v = fst . head . Map.assocs . (Map.filter (==v))

int :: Parser Int
int = fromInteger <$> integer

mapValue :: Eq a => Map.Map String a -> a -> Parser String
mapValue m v = try (spaces *> string (getKey v m) <* spaces)

unOp op = Prefix $ UnaryExpression op <$ (mapValue unaryOperations op)

binOp op = Infix (BinaryExpression op <$ (mapValue binaryOperations op)) AssocLeft

opers = [[unOp Not, unOp Neg],
        [binOp Mul, binOp Div],
        [binOp Sum, binOp Sub],
        [binOp L, binOp B, binOp BE, binOp LE],
        [binOp Eq, binOp NotE],
        [binOp And],[binOp Or]]

funcCall :: Parser FuncCall
funcCall = FuncCall <$> identifier <*> parens sepExpr

expr :: Parser Expr
expr = buildExpressionParser opers subExpr

subExpr :: Parser Expr
subExpr = parens expr
            <|> FCall  <$> try funcCall
            <|> VCall  <$> identifier
            <|> IntLit <$> int
            <|> DblLit <$> try float
            <|> StrLit <$> stringLiteral

sepExpr :: Parser [Expr]
sepExpr = commaSep expr

someKey :: Map.Map String m -> Parser m
someKey k = ((Map.!) k) <$> (choice . map string . Map.keys $ k)

buildType :: Parser Type
buildType = someKey buildInTypes

varDefinition :: Parser Var
varDefinition = Var <$> buildType <* wSpace <*> identifier 

varAssignment :: Parser Stmt
varAssignment = VarAssign <$> identifier <* char '=' <* wSpace <*> expr

stmt :: Parser Stmt
stmt = VarDef <$> varDefinition
         <|> varAssignment
         <|> FuncDef <$> function
         <|> Return <$> (string "return" *> spaces *> expr)
         <|> iF
         <|> while

lineSeparator :: Parser String
lineSeparator = semi

statementList :: Parser [Stmt]
statementList = endBy1 stmt lineSeparator

iF :: Parser Stmt
iF = do
    _ <- string "if"
    wSpace
    ex <- parens expr
    sl <- braces statementList
    exsl <- option [] (string "else" *> wSpace *> braces statementList)
    return $ If ex sl exsl

while :: Parser Stmt
while = do
    string "while"
    spaces
    ex <- parens expr
    sl <- braces statementList
    return $ While ex sl

typeVoid :: Parser (Maybe Type)
typeVoid = Just <$> buildType <|> Nothing <$ string "void"

function :: Parser Func
function = do
    t <- typeVoid
    spaces
    n <- identifier
    args <- parens $ commaSep $ (,) <$> buildType <* spaces <*> identifier
    br <- braces statementList
    return $ Func t n args br

asTree :: Parser ASTree
asTree = many1 function
