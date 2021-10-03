-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

module Kroha.Instructions where

import Data.Tree
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import Control.Monad (void)
import Control.Monad.Zip (mzip, mzipWith)

import Kroha.Ast
import Kroha.Scope
import Kroha.Stack
import Kroha.Types


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

type StackOffsetTree = Program (NodeId, StackRange)

link2target :: StackOffsetTree -> ScopeLink -> Target
link2target s (ElementLink (VariableDeclaration (StackVariable _ _) _) nid) = StackTarget . fromJust . lookup nid $ toList s
link2target _ (ElementLink (VariableDeclaration (RegisterVariable _ reg) _) _) = RegisterTarget reg
link2target _ (DeclarationLink declaration _) = let (VariableScope name) = dscope declaration in VariableTarget name (declType declaration)

target :: StackOffsetTree -> Scope -> RValue -> Target
target so s (AsRValue (VariableLVal var)) = link2target so . fromJust . lookup (VariableScope var) $ s
target _  _ (AsRValue (RegisterLVal reg)) = RegisterTarget reg
target _  _ (RLiteral literal)            = LiteralTarget literal

transformCond sot s (Condition (left, cmp, right)) = (target sot s left, cmp, target sot s right)

instruction :: StackOffsetTree -> FrameElement Scope -> [Instruction]
instruction _   (Kroha.Ast.Instructions f _)        = mzipWith Body (fmap void f) [0..]
instruction _   (Kroha.Ast.VariableDeclaration _ _) = [  ]

instruction sot (Kroha.Ast.If name cond t f s)      = [ Jump (BeginLabel name) (Just $ transformCond sot s cond),
                                                          Body (void f) 1,
                                                          Jump (EndLabel name) Nothing,
                                                        Label (BeginLabel name),
                                                          Body (void t) 0,
                                                        Label (EndLabel name) ]

instruction _   (Kroha.Ast.Loop name body _)        = [ Label (BeginLabel name),
                                                          Body (void body) 0,
                                                          Jump (BeginLabel name) Nothing,
                                                        Label (EndLabel name) ]

instruction _   (Kroha.Ast.Break loop _)            = [ Jump (EndLabel loop) Nothing ]
instruction sot (Kroha.Ast.Call name args s)        = [ CallI (CommonLabel name) (fmap (target sot s) args) ]
instruction sot (Kroha.Ast.Assignment l r s)        = [ Move (target sot s (AsRValue l)) (target sot s r) ]
instruction _   (Kroha.Ast.Inline asm _)            = [ Assembly asm ]


declSection :: Declaration d -> Section
declSection Frame { }            = "text"
declSection GlobalVariable { }   = "data"
declSection ConstantVariable { } = "rodata"
declSection ManualFrame { }      = "text"
declSection ManualVariable { }   = "data"


buildDeclaration :: StackOffsetTree -> Declaration Scope -> (Section, Declaration (), Tree [Instruction])
buildDeclaration sot d@(Frame _ frame _) = (declSection d, void d, instructions)
    where instructions =  instruction sot <$> selector id frame
buildDeclaration _ d = (declSection d, void d, Node [] [])

instructions :: Program (StackRange, Scope, NodeId) -> [(Section, Declaration (), Tree [Instruction])]
instructions p@(Program decls _) = fmap mapper decls
    where mapper decl = buildDeclaration sot $ fmap (\(_, x, _) -> x) decl
          sot = fmap (\(st, _, i) -> (i, st)) p
