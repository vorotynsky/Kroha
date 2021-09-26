-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

{-# LANGUAGE DeriveFunctor #-}

module Kroha.Ast where

import Data.Tree
import Data.List (mapAccumR)

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

data FrameElement d
    = Instructions [FrameElement d] d
    | VariableDeclaration LocalVariable d
    | If Label Condition (FrameElement d) (FrameElement d) d
    | Loop Label (FrameElement d) d
    | Break Label d
    | Call Label [RValue] d
    | Assignment LValue RValue d
    | Inline InlinedCode d
    deriving (Show, Eq, Functor)


data Declaration d
    = Frame Label (FrameElement d) d
    | GlobalVariable VariableName TypeName Literal d
    | ConstantVariable VariableName TypeName Literal d
    | ManualFrame Label InlinedCode d
    | ManualVariable VariableName TypeName InlinedCode d
    deriving (Show, Eq, Functor)

data Program d = Program [Declaration d] d
    deriving (Show, Eq, Functor)

type Selector d a = FrameElement d -> a

childs :: FrameElement d -> [FrameElement d]
childs (Instructions xs _)       = xs 
childs (VariableDeclaration x _) = []
childs (If _ _ b e _)            = [b, e]
childs (Loop _ b _)              = [b]
childs (Break _ _)               = []
childs (Call _ _ _)              = []
childs (Assignment _ _ _)        = []
childs (Inline _ _)              = []

selector :: Selector d a -> FrameElement d -> Tree a
selector s = unfoldTree (\e -> (s e, childs e))

selectorProg :: (Declaration d -> a) -> Selector d a -> Program d -> Forest a
selectorProg df sf (Program declarations _) = fmap mapper declarations
    where mapper d@(Frame _ frame _)        = Node (df d) [selector sf frame]
          mapper declaration                = Node (df declaration) []


type NodeId = Int

genId :: Tree a -> Tree NodeId
genId = snd . mapAccumR (\ac b -> (ac + 1, ac)) 0

progId :: Program d -> Tree NodeId
progId program = genId $ Node () (selectorProg (const ()) (const ()) program)
