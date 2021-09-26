-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Kroha.Ast where

import Data.Tree
import Data.List (mapAccumR)
import Data.Functor (($>))

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
    deriving (Show, Eq, Functor, Foldable, Traversable)


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

getDeclData :: Declaration d -> d
getDeclData (Frame _ _ d)              = d
getDeclData (GlobalVariable _ _ _ d)   = d
getDeclData (ConstantVariable _ _ _ d) = d
getDeclData (ManualFrame _ _ d)        = d
getDeclData (ManualVariable _ _ _ d)   = d

getFrameElementData :: FrameElement d -> d
getFrameElementData (Instructions _ d)        = d
getFrameElementData (VariableDeclaration _ d) = d
getFrameElementData (If _ _ _ _ d)            = d
getFrameElementData (Loop _ _ d)              = d
getFrameElementData (Break _ d)               = d
getFrameElementData (Call _ _ d)              = d
getFrameElementData (Assignment _ _ d)        = d
getFrameElementData (Inline _ d)              = d


selector :: Selector d a -> FrameElement d -> Tree a
selector s = unfoldTree (\e -> (s e, childs e))

selectorProg :: (Declaration d -> a) -> Selector d a -> Program d -> Forest a
selectorProg df sf (Program declarations _) = fmap mapper declarations
    where mapper d@(Frame _ frame _)        = Node (df d) [selector sf frame]
          mapper declaration                = Node (df declaration) []


type NodeId = Int

genId :: Program d -> Program NodeId
genId (Program decls _) = Program (snd $ mapAccumR declId 1 decls) 0
    where genId'' = mapAccumR (\ac b -> (ac + 1, ac))
          declId begin (Frame l fe _) = let (acc, fe') = genId'' (begin + 1) fe in (acc, Frame l fe' begin)
          declId begin d  = (begin + 1, d $> begin)

progId :: Program d -> Tree NodeId
progId program = Node 0 $ selectorProg getDeclData getFrameElementData (genId program)


duplicate :: FrameElement a -> FrameElement (FrameElement a)
duplicate node@(Instructions c _)        = Instructions (map duplicate c) node
duplicate node@(VariableDeclaration v _) = VariableDeclaration v node
duplicate node@(If l c i e _)            = If l c (duplicate i) (duplicate e) node
duplicate node@(Loop l b _)              = Loop l (duplicate b) node
duplicate node@(Break l _)               = Break l node
duplicate node@(Call l a _)              = Call l a node
duplicate node@(Assignment l r _)        = Assignment l r node
duplicate node@(Inline c _)              = Inline c node
