-- Copyright (c) 2020 Vorotynsky Maxim

{-# LANGUAGE FlexibleContexts #-}

module HLasm.Parser (HLasm.Parser.parse) where

import           HLasm.Ast
import           HLasm.Error

import           Data.Bifunctor     (first)
import           Data.Maybe         (maybeToList)
import           Data.Tree          (Tree (..), unfoldTreeM)
import           Data.Tuple.Extra   (curry3)
import           Text.Parsec
import           Text.Parsec.String (Parser (..))


treeParser :: (b -> Parser (a, [b])) -> Parser b -> Parser (Tree a)
treeParser f p = p >>= unfoldTreeM f

leafP f = treeParser (\x -> pure (f x, []))

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

break   = leafP Break        (keyword "break" *> parens name)
asmCall = leafP AssemblyCall (spaces *> char '!' *> many1 (noneOf ";") <* char ';' <* spaces)
call    = leafP id (Call <$> (keyword "call" *> angles name) <*> parens (name `sepBy` achar ','))

register = leafP id . aparse $ curry (VariableDeclaration . Register) <$> (keyword "reg" *> name) <*> (achar ':' *> name )
variable = leafP id . aparse $ curry (VariableDeclaration . Variable) <$> (keyword "var" *> name) <*> (achar ':' *> vtype)
    where vtype = Type <$> name <*> (Just <$> parens nat)

value = (IntegerValue <$> aparse nat) <|> (NameValue <$> aparse name)

assignment = leafP id (Assignment <$> name <*> (achar '=' *> value))
condition = curry3 Condition <$> value <*> cond <*> value
    where p x s = const x <$> string s
          cond = p Equals "=="  <|> p NotEquals "!=" <|> p Greater "<" <|> p Less ">"

instrSet p = ftree <$> treeParser internal (aparse . many $ aparse p)
    where internal x = pure (Node InstructionSet x, [])
block = braces . instrSet

frame p = (\a b -> Node (Frame a) [b]) <$> fname <*> block p
    where fname = (keyword "frame" *> optionMaybe (parens name))

ifstatment p =
    do (label, ifb) <- ifblock
       elseifs <- many $ elseif
       elseblk <- optionMaybe $ elseblk
       return $ Node (If label) (ifb : elseifs ++ maybeToList elseblk)
    where returnBlock cond = block p >>= return . Node (IfBranch cond) . pure
          ifblock = do keyword "if"
                       (condition, label) <- parens ((,) <$> condition <*> (achar ',' *> name))
                       block <- returnBlock $ Just condition
                       return $ (label, block)
          elseif  = do keyword "else"; keyword "if"
                       condition <- parens condition
                       returnBlock $ Just condition
          elseblk =    keyword "else" >>= const (returnBlock Nothing)

whileHead = keyword "while" *> parens name
while p   = (\l b -> Node (While   l) [b]) <$> whileHead <*> (block p)
dowhile p = (\b l -> Node (DoWhile l) [b]) <$> (spaces    *> string "do" *> block p) <*> whileHead

hlasm = reduce [ asmCall,             call,           HLasm.Parser.break,
                 register,            variable,       frame hlasm,
                 ifstatment hlasm,    while hlasm,    dowhile hlasm, 
                 assignment ]
    where reduce (x:xs) = foldl (<|>) x xs

parse :: String -> Result SyntaxTree
parse = first ParseError . Text.Parsec.parse hlasm ""
