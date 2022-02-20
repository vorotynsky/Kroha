module Kroha.Syntax.Primitive where

type VariableName = String
type RegisterName = String
type InlinedCode  = String
type Label = String

data TypeName 
    = TypeName String
    | PointerType TypeName    
    deriving (Show, Eq)

data Literal
    = IntegerLiteral Int
    deriving (Show, Eq)

data LValue
    = VariableLVal VariableName
    | RegisterLVal RegisterName
    deriving (Show, Eq)

data RValue
    = AsRValue LValue
    | RLiteral Literal
    deriving (Show, Eq)

data LocalVariable
    = StackVariable VariableName TypeName
    | RegisterVariable VariableName RegisterName
    deriving (Show, Eq)

data Comparator
    = Equals    | NotEquals
    | Greater   | Less
    deriving (Show, Eq)

newtype Condition = Condition (RValue, Comparator, RValue)
    deriving (Show, Eq)

data Range = Range
    { begin :: Int
    , end   :: Int 
    } deriving (Show, Eq)

data RegPurpose 
    = General | Argument Int | ReturnValue | StackBase | StackPointer
    deriving (Show, Eq)
