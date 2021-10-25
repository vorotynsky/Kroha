-- Copyright (c) 2021 Vorotynsky Maxim

module Kroha.Parser.Lexer where

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void (Void)
import Kroha.Syntax
import Kroha.Types (TypeConfig(literalType))


type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

nat :: Parser Int
nat = lexeme L.decimal

krP p = do
    begin <- getSourcePos
    x <- p
    end <- getSourcePos
    return $ x (begin, end)

around l r = between (symbol l) (symbol r)

parens = around "(" ")"
angles = around "<" ">"
braces = around "{" "}"

name, name' :: Parser String
name' = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
name = lexeme (name' <?> "variable")

literal = IntegerLiteral <$> nat
lvalue = (VariableLVal <$> name) <|> (RegisterLVal <$> lexeme (char '%' *> name'))
rvalue = (RLiteral <$> literal) <|> (AsRValue <$> lvalue)

typeName = (PointerType <$> lexeme (char '&' *> typeName)) <|> (TypeName <$> name)

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
