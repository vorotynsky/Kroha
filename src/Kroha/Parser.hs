-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

{-# LANGUAGE FlexibleContexts #-}

module Kroha.Parser (Kroha.Parser.parse) where

import           Kroha.Ast
import           Kroha.Errors

import           Data.Bifunctor     (first)
import           Data.Maybe         (maybeToList)
import           Data.Tree          (Tree (..), unfoldTreeM)
import           Data.Tuple.Extra   (curry3)
import           Text.Parsec
import           Text.Parsec.String (Parser (..))


nat :: Parser Int
nat = fmap read (many1 digit)

aparse :: Parser a -> Parser a
aparse  = let spaces = many space in between spaces spaces

krP p = do
    begin <- getPosition
    value <- p
    end <- getPosition
    return $ value (begin, end)

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

break  = krP $ Break  <$> (keyword "break" *> parens name)
inline = krP $ Inline <$> aparse (char '!' *> many (noneOf "\n"))
call   = krP $ Call   <$> (keyword "call" *> angles name) <*> parens (rvalue `sepBy` achar ',')

vtype = PointerType <$> (achar '&' *> vtype)
    <|> TypeName <$> name

register = krP . aparse $ VariableDeclaration <$> (RegisterVariable <$> (keyword "reg" *> name) <*> (achar ':' *> name ))
variable = krP . aparse $ VariableDeclaration <$> (StackVariable    <$> (keyword "var" *> name) <*> (achar ':' *> vtype))

assignment = krP $ Assignment <$> lvalue <*> (achar '=' *> rvalue)

condition = curry3 Condition <$> rvalue <*> cond <*> rvalue
    where p x s = const x <$> string s
          cond = p Equals "=="  <|> p NotEquals "!=" <|> p Greater ">" <|> p Less "<"

instrSet p = Instructions <$> (aparse . many $ aparse p)
block = krP . braces . instrSet

loop p = krP $ Loop <$> (keyword "loop" *> parens name) <*> block p

ifstatment p = krP $
    do keyword "if"
       (cond, label) <- parens $ (,) <$> (condition <* achar ',') <*> name
       body <- block p
       elseb <- (keyword "else" *> block p) <|> krP (pure (Instructions []))
       return $ If label cond body elseb

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

globals = krP $ frame hlasm <|> constant <|> globvar <|> manualFrame <|> manualVar
program = krP $ fmap Program parser
    where parser = keyword "program" *> braces (many globals)


parse :: String -> String -> Either String (Program (SourcePos, SourcePos))
parse name = first show . Text.Parsec.parse program name
