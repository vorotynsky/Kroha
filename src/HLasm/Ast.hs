-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Ast where


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

data IfBranch f = IfBranch (Condition, f)
    deriving (Show, Eq)

data HLElement = 
    InstructionSet [HLElement]
    | VariableDeclaration HLValuable
    | Frame (Maybe Label) HLElement
    | If { mainIf    :: IfBranch HLElement
         , label     :: Label
         , elseIfs   :: [IfBranch HLElement]
         , elseBlock :: Maybe (HLElement) }
    | While Label HLElement
    | DoWhile Label HLElement
    | Break Label
    | Call Label [VariableName]
    | Assigment VariableName HLValue
    | AssemblyCall String
    deriving (Show, Eq)
