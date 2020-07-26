-- Copyright (c) 2020 Vorotynsky Maxim

module Kroha.Ast where

import Data.Tree

type VariableName = String
type RegisterName = String
type InlinedCode  = String
type Label = String

data TypeName 
    = TypeName String
    | SizedType String Int
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

data FrameElement
    = Instructions [FrameElement]
    | VariableDeclaration LocalVariable
    | If Label Condition FrameElement FrameElement
    | Loop Label FrameElement
    | Break Label
    | Call Label [RValue]
    | Assignment LValue RValue
    | Inline InlinedCode 
    deriving (Show, Eq)


data Declaration
    = Frame Label FrameElement
    | GlobalVariable VariableName TypeName Literal
    | ConstantVariable VariableName TypeName Literal
    | ManualFrame Label InlinedCode
    | ManualVariable VariableName TypeName InlinedCode 
    deriving (Show, Eq)

newtype Program = Program [Declaration]
    deriving (Show, Eq)

type Selector a = FrameElement -> a

childs :: FrameElement -> [FrameElement]
childs (Instructions xs)       = xs 
childs (VariableDeclaration x) = []
childs (If _ _ b e)            = [b, e]
childs (Loop _ _)              = []
childs (Break _)               = []
childs (Call _ _)              = []
childs (Assignment _ _)        = []
childs (Inline _)              = []

selector :: Selector a -> FrameElement -> Tree a
selector s = unfoldTree (\e -> (s e, childs e))

selectorM :: Monad m => Selector (m a) -> FrameElement -> m (Tree a)
selectorM s = unfoldTreeM (\e -> s e >>= (\x -> return (x, childs e)))
