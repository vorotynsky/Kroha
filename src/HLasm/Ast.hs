-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Ast where

import Data.Tree

data Type = Type
    { typeName :: String
    , typeSize :: Maybe Int }
    deriving (Show, Eq)

type VariableName = String
type RegisterName = String
type Label = String

data CompareType = 
    Equals
    | NotEquals
    | Greater
    | Less
    deriving (Show, Eq)

data HLValuable = 
    Variable (VariableName, Type)
    | Register (VariableName, RegisterName)
    deriving (Show, Eq)

data HLValue = 
    NameValue VariableName
    | IntegerValue Int
    | StringValue String
    deriving (Show, Eq)

data Condition = Condition (HLValue, CompareType, HLValue)
    deriving (Show, Eq)

data HLElement = 
    InstructionSet
    | VariableDeclaration HLValuable
    | Frame (Maybe Label)
    | If Label
    | IfBranch (Maybe Condition)
    | While Label
    | DoWhile Label
    | Break Label
    | Call Label [VariableName]
    | Assigment VariableName HLValue
    | AssemblyCall String
    deriving (Show, Eq)

type SyntaxTree = Tree HLElement
