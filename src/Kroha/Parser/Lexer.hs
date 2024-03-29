-- Copyright (c) 2021 Vorotynsky Maxim

module Kroha.Parser.Lexer where

import           Control.Monad              (void)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Kroha.Syntax.Primitive


type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

end :: Parser ()
end = void . lexeme . optional . char $ ';'

nat :: Parser Int
nat = lexeme L.decimal

krP p = do
    begin <- getSourcePos
    x <- p
    end <- getSourcePos
    return $ x (begin, end)

around l r = between (symbol l) (symbol r)

parens = around "(" ")"
braces = around "{" "}"
parensOpt p = parens p <|> p


name, name' :: Parser String
name' = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
name = lexeme (name' <?> "identifier")

callName = around "<" ">" name <|> name

literal = IntegerLiteral <$> nat <?> "integer literal"
lvalue = (VariableLVal <$> name <?> "variable name") <|> (RegisterLVal <$> lexeme (char '%' *> name') <?> "register name")
rvalue = (RLiteral <$> literal) <|> (AsRValue <$> lvalue)

typeName = (PointerType <$> lexeme (char '&' *> typeName)) <|> (TypeName <$> name) <?> "type name"
typeSpecification = symbol ":" *> typeName <?> "type specification"

-- keywords --
break'         = symbol "break"
call'          = symbol "call"
if'            = symbol "if"
else'          = symbol "else"
loop'          = symbol "loop"
reg'           = symbol "reg"
var'           = symbol "var"
frame'         = symbol "frame"
const'         = symbol "const"
manual'        = symbol "manual"
program'       = symbol "program"
