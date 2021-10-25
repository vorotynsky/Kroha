-- Copyright (c) 2021 Vorotynsky Maxim

module Kroha.Parser.Declarations where

import Kroha.Parser.Lexer
import Kroha.Syntax (Declaration(..), Program (Program))
import Text.Megaparsec
import Data.Bifunctor (first)
import Kroha.Parser.Statements (body, statement)

globalVariable w f = f <$> (w *> name) <*> (symbol ":" *> typeName) <*> (symbol "=" *> literal)

constant = globalVariable const' ConstantVariable
variable = globalVariable var'   GlobalVariable

manual w d = do
             try (manual' *> w)
             def <- d
             code <- braces (many (noneOf "}"))
             return (def code)

manualFrame = manual frame' (ManualFrame    <$> name)
manualVar   = manual var'   (ManualVariable <$> name <*> (symbol ":" *> typeName))

frame = Frame <$> (frame' *> name) <*> body statement 

globals = krP $ choice [constant, variable, manualFrame, manualVar, frame]

program = krP $ Program <$> (program' *> braces (many globals))

parseProgram :: String -> String -> Either String (Program (SourcePos, SourcePos))
parseProgram name = first errorBundlePretty . runParser program name
