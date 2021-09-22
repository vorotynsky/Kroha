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


data Declaration d
    = Frame Label FrameElement d
    | GlobalVariable VariableName TypeName Literal d
    | ConstantVariable VariableName TypeName Literal d
    | ManualFrame Label InlinedCode d
    | ManualVariable VariableName TypeName InlinedCode d
    deriving (Show, Eq, Functor)

data Program d = Program [Declaration d] d
    deriving (Show, Eq, Functor)

type Selector a = FrameElement -> a

childs :: FrameElement -> [FrameElement]
childs (Instructions xs)       = xs 
childs (VariableDeclaration x) = []
childs (If _ _ b e)            = [b, e]
childs (Loop _ b)              = [b]
childs (Break _)               = []
childs (Call _ _)              = []
childs (Assignment _ _)        = []
childs (Inline _)              = []

selector :: Selector a -> FrameElement -> Tree a
selector s = unfoldTree (\e -> (s e, childs e))

selectorM :: Monad m => Selector (m a) -> FrameElement -> m (Tree a)
selectorM s = unfoldTreeM (\e -> s e >>= (\x -> return (x, childs e)))


selectorProg :: (Declaration d -> a) -> Selector a -> Program d -> Forest a
selectorProg df sf (Program declarations _) = fmap mapper declarations
    where mapper d@(Frame _ frame _)        = Node (df d) [selector sf frame]
          mapper declaration                = Node (df declaration) []


type NodeId = Int

genId :: Tree a -> Tree NodeId
genId = snd . mapAccumR (\ac b -> (ac + 1, ac)) 0

progId :: Program d -> Tree NodeId
progId program = genId $ Node () (selectorProg (const ()) (const ()) program)
