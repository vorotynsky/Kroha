-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Ast where

import Data.Tree
import Data.Maybe

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

data LValue =
    NameValue VariableName
    deriving (Eq)

data RValue =
    LeftValue LValue
    | IntegerValue Int
    deriving (Eq)

instance Show LValue where
    show (NameValue     name) = name

instance Show RValue where
    show (LeftValue  value) = show value
    show (IntegerValue num) = show num

data Condition = Condition (RValue, CompareType, RValue)
    deriving (Show, Eq)

data HLElement = 
    Program
    | InstructionSet
    | RegisterDeclaration  VariableName RegisterName
    | VariableDeclaration  VariableName Type
    | GlobalVarDeclaration VariableName Type RValue
    | ConstVarDeclaration  VariableName Type RValue
    | FakeVariable         VariableName
    | FakeFrame            Label
    | Frame (Maybe Label)
    | If Label
    | IfBranch (Maybe Condition)
    | While Label
    | DoWhile Label
    | Break Label
    | Call Label [RValue]
    | Assignment LValue RValue
    | AssemblyCall String
    deriving (Show, Eq)

getValuableName :: HLElement -> Maybe VariableName
getValuableName (VariableDeclaration  name _  ) = Just name
getValuableName (RegisterDeclaration  name _  ) = Just name
getValuableName (GlobalVarDeclaration name _ _) = Just name
getValuableName (ConstVarDeclaration  name _ _) = Just name
getValuableName _                               = Nothing

isVariable :: HLElement -> Bool
isVariable = isJust . getValuableName

variableName :: HLElement -> VariableName
variableName = fromJust . getValuableName

variableNameValue :: RValue -> Maybe VariableName
variableNameValue (LeftValue (NameValue name)) = Just name
variableNameValue _                            = Nothing

usedVariables :: HLElement -> [VariableName]
usedVariables (IfBranch (Just (Condition(left, _, right)))) = name left ++ name right
    where name (LeftValue (NameValue name)) = [name]
          name _                = []
usedVariables (Call _ xs)                         = fromMaybe [] $ traverse variableNameValue xs
usedVariables (Assignment left (LeftValue right)) = fromMaybe [] $ traverse (variableNameValue . LeftValue) [left, right]
usedVariables (Assignment left _)                 = fromMaybe [] $ traverse (variableNameValue . LeftValue) [left]
usedVariables _                                   = []

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
