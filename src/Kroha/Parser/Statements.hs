-- Copyright (c) 2021 Vorotynsky Maxim

module Kroha.Parser.Statements where

import Kroha.Syntax.Syntax
import Kroha.Parser.Lexer
import Text.Megaparsec
import Data.Tuple.Extra (curry3)
import Text.Megaparsec.Char (space1)

break  = krP $ Break  <$> (break' *> parensOpt name)
inline = krP (Inline <$> (symbol "!" *> many (noneOf "\n"))) <* space1
call   = krP $ Call   <$> (call' *> callName) <*> parens (rvalue `sepBy` symbol ",")

register = krP $ VariableDeclaration <$> (RegisterVariable <$> (reg' *> name) <*> (symbol ":" *> (name <?> "register name")))
variable = krP $ VariableDeclaration <$> (StackVariable    <$> (var' *> name) <*> typeSpecification)

assignment = krP $ Assignment <$> try (lvalue <* symbol "=") <*> rvalue

body pStatement = krP $ braces (Instructions <$> many pStatement)
body' pStatement = body pStatement <|> pStatement

ifStatement pStatement = krP $
    do if'
       (condition, label) <- parens $ (,) <$> (pCondition <* symbol ",") <*> name
       body <- body' pStatement
       elze <- (else' *> body' pStatement) <|> krP (pure (Instructions []))
       return $ If label condition body elze
    where pCondition = curry3 Condition <$> rvalue <*> cmpToken <*> rvalue
          p x s = x <$ symbol s
          cmpToken = p Equals "=="  <|> p NotEquals "!=" <|> p Greater ">" <|> p Less "<"

loop ps = krP $ Loop <$> (loop' *> parensOpt name) <*> body' ps

statement = recover (choice ( fmap (<* Kroha.Parser.Lexer.end)
                     [ inline, call, Kroha.Parser.Statements.break,
                       register, variable, assignment ] 
                     ++
                     [ ifStatement statement, loop statement ]
                   )
            <?> "statement")
    where recover = withRecovery $ \e -> do
            registerParseError e
            krP (skip ";\n}")
          skip s = do some (noneOf s)
                      oneOf s
                      return $ Instructions []
