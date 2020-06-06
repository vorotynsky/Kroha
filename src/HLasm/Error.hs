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
    | GlobalVariableInFrame VariableName
    deriving (Eq)

type Result a = Either Error a

instance Show Error where
    show (StringError msg) = "error: " ++ msg ++ ".\n"
    show (VariableNotFound name) = "scope error: a variable \'" ++ name ++ "\' not found.\n"
    show (LabelNotFound label) = "scope error: a label \'" ++ label ++ "\' not found.\n"
    show (IncompatibleTypes (left, right)) = 
        "type error: incompatible types between \'" ++ show left ++ "\' and \'" ++ show right ++ "\'.\n"
    show (ParseError err) = "parser error, " ++ show err
    show (GlobalVariableInFrame name) = "error: the global variable \'" ++ name ++ "\' isn't in the global scope.\n"
