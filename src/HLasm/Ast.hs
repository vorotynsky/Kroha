-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Ast where 


data Type = Type 
    { typeName :: String
    , typeSize :: Int }
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

type Condition e = (e, CompareType, e)

data HLValue =
    Variable  (VariableName, Type)
    | Register  (VariableName, RegisterName)
    | Value     String
    deriving (Show, Eq)

data IfBranch f = IfBranch (Label, Condition HLValue, f)
    deriving (Show, Eq)

data IfNode f = IfNode 
    { mainIf :: IfBranch f
    , elseIfs :: [IfBranch f]
    , elseBlock :: Maybe (f, Label) }
    deriving (Show, Eq)

data HLElement =
    InstructionSet  [HLElement]
    | VariableDeclaration HLValue
    | Frame (Maybe Label) HLElement
    | If  (IfNode HLElement)
    | While  Label HLElement
    | DoWhile  Label HLElement
    | Break  Label
    | Call  Label [HLValue]
    | Assigment  VariableName (Either VariableName String)
    | AssembleyCall String
    deriving (Show, Eq)
