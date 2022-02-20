-- Copyright (c) 2021 Vorotynsky Maxim

module Kroha.Parser.Declarations where

import Kroha.Parser.Lexer
import Kroha.Parser.Statements (body, statement)
import Kroha.Syntax.Declarations (Declaration(..), Program (Program), Register (Register))
import Kroha.Syntax.Primitive (RegPurpose(General))
import Text.Megaparsec
import Data.Bifunctor (first)
import Control.Monad (void)

globalVariable w f = f <$> (w *> name) <*> typeSpecification <*> (symbol "=" *> literal)

constant = globalVariable const' ConstantVariable
variable = globalVariable var'   GlobalVariable

manuals ps = do
             manual'
             decl <- foldl (<|>) empty ps
             code <- braces (many (noneOf "}"))
             return (decl code)

manualDeclarations = manuals 
    [ ManualFrame    <$> (frame' *> name)
    , ManualVariable <$> (var'   *> name) <*> typeSpecification]

frame = Frame <$> (frame' *> name) <*> body statement 


regDef = do
    symbol "register"
    rName <- name
    for <- (symbol "for" *> registerPurpose) <|> pure General
    layout <- braces (many (reg <* Kroha.Parser.Lexer.end))

    return (Register rName for layout)
    where reg = (,) <$> (name <* symbol ":") <*> range

regDeclaration = fmap RegisterDeclaration regDef

globals = recover (choice (fmap krP [constant, variable, manualDeclarations, frame, regDeclaration]) <?> "declaration")
    where recover = withRecovery $ \e -> do
                        registerParseError e
                        krP skip
          skip = do someTill (satisfy (const True)) (const' <|> var' <|> manual' <|> frame') 
                    return (ManualFrame "" "'")

program = krP $ Program <$> prog (many globals) <* endOfFile
    where prog p = (program' *> (braces p <|> symbol ";" *> p)) <|> p
          endOfFile = eof <|> void (symbol "=======" *> lexeme (some (noneOf "=") <* symbol "=======")) <?> "end of file"

parseProgram :: String -> String -> Either String (Program (SourcePos, SourcePos))
parseProgram name = first errorBundlePretty . runParser program name
