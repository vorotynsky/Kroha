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

valuableName :: HLValuable -> VariableName
valuableName (Variable (name, _)) = name
valuableName (Register (name, _)) = name

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

usedVariables :: HLElement -> [VariableName]
usedVariables (IfBranch (Just (Condition(left, _, right)))) = name left ++ name right
    where name (NameValue name) = [name]
          name _                = []
usedVariables (Call _ xs)                        = xs
usedVariables (Assigment left (NameValue right)) = [left, right]
usedVariables (Assigment left _)                 = [left]
usedVariables _                                  = []

usedLabels :: HLElement -> [Label]
usedLabels (Break label)  = [label]
usedLabels (Call label _) = [label]
usedLabels _              = []

type SyntaxTree = Tree HLElement

ftree :: Tree (Tree a) -> Tree a
ftree (Node t [])         = t
ftree (Node (Node t f) x) = Node t (f ++ fmap ftree x)

nestedTree :: Monoid a => [Tree a] -> Tree a
nestedTree xs = ftree $ unfoldTree internal xs
    where internal []     = (Node mempty [], [])
          internal [x]    = (x, [])
          internal (x:xs) = (x, [xs])
