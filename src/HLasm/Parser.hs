-- Copyright (c) 2020 Vorotynsky Maxim

{-# LANGUAGE FlexibleContexts #-}

module HLasm.Parser{-(parse)-}  where

import           HLasm.Ast

import           Control.Applicative
import           Data.Char
import           Data.Tree
import           Data.Maybe
import           Data.List.NonEmpty

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

treeParser :: (b -> Parser (a, [b])) -> Parser b -> Parser (Tree a)
treeParser f p = p >>= unfoldTreeM f

ftree :: Tree (Tree a) -> Tree a
ftree (Node t [])  = t
ftree (Node (Node t f) x) = Node t (f ++ fmap ftree x)

leafP f = treeParser (\x -> pure (f x, []))
nodeP f parg = treeParser (\i -> (\x -> (f x, [i])) <$> parg)

--------- Parsing ---------
achar   = aws . char
astring = aws . string

name    = (aws . some) (predicate isAlpha)
braked  = aws . (between (char '(') (char ')'))
literal = aws (around (char '\"') (some $ predicate (/= '\"')))

instrSet p = ftree <$> treeParser internal (aws . many $ aws p)
    where internal x     = pure (Node InstructionSet x, [])
block p    = achar '{' *> instrSet p <* achar '}'

asmCall = leafP AssemblyCall (ws *> char '!' *> some (predicate (/= '\n')) <* char '\n')
break   = leafP Break (astring "break" *> braked name)
call    = leafP id $ Call <$> (ws *> string "call" *> name) <*> braked (stepBy (char ',') name)

register = leafP id . aws $ curry (VariableDeclaration . Register) <$> (string "reg" *> name) <*> (achar '=' *> some (predicate isAlpha))
variable = leafP id . aws $ curry (VariableDeclaration . Variable) <$> (string "var" *> name) <*> (achar ':' *> vtype)
        where vtype = Type <$> name <*> (Just <$> braked nat)

value = (IntegerValue <$> aws nat) <|> (StringValue <$> literal) <|> (NameValue <$> name)
assigment = leafP id $ Assigment <$> name <*> (achar '=' *> value)

frame p = (\a b -> Node (Frame a) [b]) <$> fname <*> (block p)
    where fname = (ws *> string "frame") *> opt (braked name)

condition = (\a c b -> Condition (a, c, b)) <$> value <*> cond <*> value
    where p x s = const x <$> string s
          cond = p Equals "=="  <|> p NotEquals "!=" <|> p Greater "<" <|> p Less ">"

ifstatment p = (\(l, a) b c -> Node (If l) (b ++ maybeToList c)) <$> (ifblock >>= (\(l, e) -> (,) l <$> treefy (pure e))) <*> many (treefy elseif) <*> opt (treefy elseb)
    where treefy el = (\a b -> Node a [b]) <$> el <*> (block p)
          ifblock   = ((\(c, l) -> (l, IfBranch $ Just c)) <$> (astring "if" *> (braked $ (,) <$> (condition <* char ',') <*> name)))
          elseif    = (IfBranch . Just) <$> (ws *> string "else" *> ws1 *> string "if" *> braked (aws condition))
          elseb     = (const $ IfBranch Nothing) <$> (astring "else")


whileHead = ws *> string "while" *> braked name
while p   = (\l b -> Node (While l) [b]) <$> whileHead <*> (block p)
dowhile p = (\b l -> Node (DoWhile l) [b]) <$> (ws *> string "do" *> block p) <*> whileHead

hlasm = reduce [ asmCall,       call,      HLasm.Parser.break,
                 register,      variable,  assigment,
                 frame hlasm,   ifstatment hlasm,
                 while hlasm,   dowhile    hlasm ]
    where reduce (x:xs) = foldl (<|>) x xs

f a = putStr $ drawTree . fmap (show) . snd. fromJust $ a
    where fromJust (Just x) = x

parse :: String -> Maybe SyntaxTree
parse = fmap snd . runParser hlasm
-- parse = \x -> Nothing
 
