-- Copyright (c) 2021 Vorotynsky Maxim

module Kroha.Parser.Declarations where

import Kroha.Parser.Lexer
import Kroha.Syntax (Declaration(..), Program (Program))
import Text.Megaparsec
import Data.Bifunctor (first)
import Kroha.Parser.Statements (body, statement)

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

globals = recover (choice (fmap krP [constant, variable, manualDeclarations, frame]) <?> "declaration")
    where recover = withRecovery $ \e -> do
                        registerParseError e
                        krP skip
          skip = do someTill (satisfy (const True)) (const' <|> var' <|> manual' <|> frame') 
                    return (ManualFrame "" "'")

program = krP $ Program <$> (program' *> braces (many globals))

parseProgram :: String -> String -> Either String (Program (SourcePos, SourcePos))
parseProgram name = first errorBundlePretty . runParser program name
