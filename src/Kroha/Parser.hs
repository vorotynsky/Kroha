-- Copyright (c) 2020 Vorotynsky Maxim

{-# LANGUAGE FlexibleContexts #-}

module Kroha.Parser (Kroha.Parser.parse) where

import           Kroha.Ast

import           Data.Bifunctor     (bimap)
import           Data.Maybe         (maybeToList)
import           Data.Tree          (Tree (..), unfoldTreeM)
import           Data.Tuple.Extra   (curry3)
import           Text.Parsec
import           Text.Parsec.String (Parser (..))


nat :: Parser Int
nat = fmap read (many1 digit)

aparse :: Parser a -> Parser a
aparse  = let spaces = many space in between spaces spaces

achar   = aparse . char
keyword word = aparse $ try (string word <* space)

around :: Char -> Char -> Parser a -> Parser a
around l r = between (achar l) (achar r)

parens = around '(' ')'
angles = around '<' '>'
braces = around '{' '}'

name :: Parser String
name = (:) <$> letter <*> many (alphaNum <|> char '_')

literal = IntegerLiteral <$> nat
literal' = aparse literal
lvalue' = (VariableLVal <$> name) <|> (RegisterLVal <$> (char '%' *> name))
lvalue = aparse lvalue'
rvalue' = (RLiteral<$> literal) <|> (AsRValue <$> lvalue')
rvalue = aparse rvalue'

break  = Break  <$> (keyword "break" *> parens name)
inline = Inline <$> aparse (char '!' *> many (noneOf "\n"))
call   = Call   <$> (keyword "call" *> angles name) <*> parens (rvalue `sepBy` achar ',')

vtype = PointerType <$> (achar '&' *> vtype)
    <|> SizedType <$> name <*> (parens nat)
    <|> TypeName <$> name  

register = aparse $ VariableDeclaration <$> (RegisterVariable <$> (keyword "reg" *> name) <*> (achar ':' *> name ))
variable = aparse $ VariableDeclaration <$> (StackVariable    <$> (keyword "var" *> name) <*> (achar ':' *> vtype))

assignment = Assignment <$> lvalue <*> (achar '=' *> rvalue)

condition = curry3 Condition <$> rvalue <*> cond <*> rvalue
    where p x s = const x <$> string s
          cond = p Equals "=="  <|> p NotEquals "!=" <|> p Greater ">" <|> p Less "<"

instrSet p = Instructions <$> (aparse . many $ aparse p)
block = braces . instrSet

loop p = Loop <$> (keyword "loop" *> parens name) <*> (block p)

ifstatment p =
    do keyword "if"
       (cond, label) <- parens $ (,) <$> (condition <* achar ',') <*> name
       body <- block p
       elseb <- (keyword "else" *> block p) <|> (pure $ Instructions [])
       return $ If label cond body (elseb)

hlasm = reduce [ inline,              call,           Kroha.Parser.break,
                 register,            variable,
                 ifstatment hlasm,    loop hlasm,     assignment ]
    where reduce (x:xs) = foldl (<|>) x xs

globalVariable word f = aparse $ f <$> (keyword word *> name) <*> (achar ':' *> vtype) <*> (achar '=' *> literal')
constant = globalVariable "const" ConstantVariable
globvar  = globalVariable "var"   GlobalVariable

manual w d = do (try . aparse) (string "manual" *> spaces *> string w)
                def <- d
                code <- braces (many (satisfy (/= '}')))
                return (def code)
            where braces = between (spaces *> char '{') (char '}' <* spaces)

manualFrame = manual "frame" (ManualFrame <$> (aparse $ name))
manualVar = manual "var" (ManualVariable <$> (aparse $ name) <*> (achar ':' *> vtype))

frame p = Frame <$> fname <*> block p
    where fname = (keyword "frame" *> name)

globals = frame hlasm <|> constant <|> globvar <|> manualFrame <|> manualVar
program = keyword "program" *> braces (many globals)

parse :: String -> Either String Program
parse = bimap show Program . Text.Parsec.parse program ""
