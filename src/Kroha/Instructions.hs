-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

module Kroha.Instructions where

import Data.Tree
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import Control.Monad.Zip (mzip, mzipWith)

import Kroha.Ast
import Kroha.Scope
import Kroha.Stack
import Kroha.Types
import Control.Monad


type Section = String

data Target
    = LiteralTarget Literal
    | StackTarget StackRange
    | RegisterTarget RegisterName
    | VariableTarget VariableName TypeName
    deriving (Show)

data LabelTarget
    = CommonLabel Label
    | BeginLabel  Label
    | EndLabel    Label
    deriving (Show)

data Instruction
    = Body (FrameElement ()) Int
    | Assembly String
    | Variable VariableName Int
    | Label LabelTarget
    | Move Target Target
    | CallI LabelTarget [Target]
    | Jump LabelTarget (Maybe (Target, Comparator, Target))
    deriving (Show)

type StackOffsetTree = Tree (NodeId, StackRange)

link2target ::  StackOffsetTree -> ScopeLink -> Target
link2target s (ElementLink (VariableDeclaration (StackVariable _ _) _) nid) = StackTarget . fromJust . lookup nid $ toList s
link2target _ (ElementLink (VariableDeclaration (RegisterVariable _ reg) _) _) = RegisterTarget reg
link2target _ (DeclarationLink declaration _) = let (VariableScope name) = dscope declaration in VariableTarget name (declType declaration)

target :: StackOffsetTree -> Scope -> RValue -> Target
target so s (AsRValue (VariableLVal var)) = link2target so . fromJust . lookup (VariableScope var) $ s
target _  _ (AsRValue (RegisterLVal reg)) = RegisterTarget reg
target _  _ (RLiteral literal)            = LiteralTarget literal

transformCond sot s (Condition (left, cmp, right)) = (target sot s left, cmp, target sot s right)

instruction :: StackOffsetTree -> Scope -> FrameElement () -> [Instruction]
instruction _   _ (Kroha.Ast.Instructions f _)        = mzipWith Body f [0..]
instruction _   _ (Kroha.Ast.VariableDeclaration _ _) = [  ]

instruction sot s (Kroha.Ast.If name cond t f _)      = [ Jump (BeginLabel name) (Just $ transformCond sot s cond),
                                                          Body f 1,
                                                          Jump (EndLabel name) Nothing,
                                                        Label (BeginLabel name),
                                                          Body t 0,
                                                        Label (EndLabel name) ]

instruction _   _ (Kroha.Ast.Loop name body _)        = [ Label (BeginLabel name),
                                                          Body body 0,
                                                          Jump (BeginLabel name) Nothing,
                                                        Label (EndLabel name) ]

instruction _   _ (Kroha.Ast.Break loop _)            = [ Jump (EndLabel loop) Nothing ]
instruction sot s (Kroha.Ast.Call name args _)        = [ CallI (CommonLabel name) (fmap (target sot s) args) ]
instruction sot s (Kroha.Ast.Assignment l r _)        = [ Move (target sot s (AsRValue l)) (target sot s r) ]
instruction _   _ (Kroha.Ast.Inline asm _)            = [ Assembly asm ]


declSection :: Declaration d -> Section
declSection Frame { }            = "text"
declSection GlobalVariable { }   = "data"
declSection ConstantVariable { } = "rodata"
declSection ManualFrame { }      = "text"
declSection ManualVariable { }   = "data"


buildDeclaration :: StackOffsetTree -> Tree Scope -> Declaration d -> (Section, Declaration d, Tree [Instruction])
buildDeclaration sot (Node _ [scope]) d@(Frame _ frame _) = (declSection d, d, instructions)
    where instructions = mzipWith (instruction sot) scope (selector id $ void frame)
buildDeclaration _ _ d = (declSection d, d, Node [] [])

instructions :: Tree StackRange -> Tree Scope -> Program d -> [(Section, Declaration d, Tree [Instruction])]
instructions offsets (Node _ scopes) p@(Program declarations _) = mzipWith (buildDeclaration sot) scopes declarations
    where sot = mzip (progId p) offsets
