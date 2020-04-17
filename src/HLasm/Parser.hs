-- Copyright (c) 2020 Vorotynsky Maxim

{-# LANGUAGE FlexibleContexts #-}

module HLasm.Parser(parse)  where

import           HLasm.Ast

import           Control.Applicative
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser fp) = Parser $ \i ->
        do
        (input, a) <- fp i
        Just (input, f a)

instance Applicative Parser where
    pure x = Parser (\i -> Just (i, x))
    (Parser fp) <*> (Parser pa) = Parser $ \i ->
        do
        (input, f) <- fp i
        (input', x) <- pa input
        Just (input', f x)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser a) <|> (Parser b) = Parser $ \i -> a i <|> b i

instance Monad Parser where
    (Parser mp) >>= f = Parser $ \i ->
        do
        (input, v) <- mp i
        runParser (f v) input

predicate :: (Char -> Bool) -> Parser Char
predicate p = Parser f
    where f []     = Nothing
          f (x:xs) = if p x then Just (xs, x) else Nothing

char :: Char -> Parser Char
char c = predicate (== c)

string :: String -> Parser String
string = traverse char

stepBy :: Parser s -> Parser a -> Parser [a]
stepBy ps pa = (\l a -> l ++ [a]) <$> many (const <$> pa <*> ps) <*> pa

ws :: Parser String
ws = many (predicate isSpace)

ws1 :: Parser String
ws1 = some (predicate isSpace)

between s1 s2 p = s1 *> p <* s2
around s = between s s
aws = around ws

opt p = fmap Just p <|> pure Nothing

nat :: Parser Int
nat = read <$> some (predicate isDigit)

--------- Parsing ---------
achar   = aws . char
astring = aws . string

name    = (aws . some) (predicate isAlpha)
braked  = aws . (between (char '(') (char ')'))
literal = aws (around (char '\"') (some $ predicate (/= '\"')))

instrSet p = InstructionSet <$> (aws . many) (aws p)
block p    = achar '{' *> instrSet p <* achar '}'

asmCall = AssemblyCall <$> (ws *> char '!' *> some (predicate (/= '\n')) <* char '\n')
break   = Break <$> (astring "break" *> braked name)
call    = Call <$> (ws *> string "call" *> name) <*> braked (stepBy (char ',') name)

register = aws $ curry (VariableDeclaration . Register) <$> (string "reg" *> name) <*> (achar '=' *> some (predicate isAlpha))
variable = aws $ curry (VariableDeclaration . Variable) <$> (string "var" *> name) <*> (achar ':' *> vtype)
        where vtype = Type <$> name <*> (braked nat)

value = (IntegerValue <$> aws nat) <|> (StringValue <$> literal) <|> (NameValue <$> name)
assigment = Assigment <$> name <*> (achar '=' *> value)

frame p = Frame <$> fname <*> block p
    where fname = (ws *> string "frame") *> opt (braked name)

condition = (\a c b -> Condition (a, c, b)) <$> value <*> cond <*> value
    where p x s = const x <$> string s
          cond = p Equals "=="  <|> p NotEquals "!=" <|> p Greater "<" <|> p Less ">"

ifstatment p = (\i ei e -> If i ei e) <$> ifblock <*> many elseif <*> opt elseb
    where branch (c, l) b = IfBranch (l, c, b)
          ifblock = astring "if" *> (branch <$> (braked $ (,) <$> (condition <* char ',') <*> name) <*> block p)
          elseif  = (ws *> string "else" *> ws1 *> ifblock)
          elseb   = astring "else" *> ((\a b -> (b, a)) <$> braked name <*> block p)

whileHead = ws *> string "while" *> braked name
while p   = While <$> whileHead <*> block p
dowhile p = (\e l -> DoWhile l e) <$> (ws *> string "do" *> block p) <*> whileHead


hlasm = reduce [ asmCall,       call,      HLasm.Parser.break,
                 register,      variable,  assigment,
                 frame hlasm,   ifstatment hlasm,
                 while hlasm,   dowhile    hlasm ]
    where reduce (x:xs) = foldl (<|>) x xs

parse :: String -> Maybe HLElement
parse = fmap snd . runParser hlasm

