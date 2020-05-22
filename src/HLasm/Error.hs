-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Error where

import HLasm.Ast
import qualified Text.Parsec

data Error = 
    StringError String
    | ParseError Text.Parsec.ParseError
    | VariableNotFound VariableName
    | LabelNotFound Label
    | IncompatibleTypes (HLValue, HLValue)
    deriving (Eq)

type Result a = Either Error a

instance Show Error where
    show (StringError msg) = "error: " ++ msg ++ ".\n"
    show (VariableNotFound name) = "scope error: variable \'" ++ name ++ "\' not found.\n"
    show (LabelNotFound label) = "scope error: label \'" ++ label ++ "\' not found.\n"
    show (IncompatibleTypes (left, right)) = 
        "type error: incompatible types between \'" ++ show left ++ "\' and \'" ++ show right ++ "\'.\n"
    show (ParseError err) = "parser error, " ++ show err
