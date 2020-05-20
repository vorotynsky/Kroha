-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Instructions
( Offset(..), Target(..), InstructionSet(..)
, Instructions(..)
, BackEnd(..)
, runBackend
, instructions
) where

import           Control.Monad.Extra (concatMapM)
import           Data.List
import           Data.Maybe
import           Data.Tree
import           HLasm.Ast
import           HLasm.Frame
import           HLasm.Scope

type Offset = Int
type Size = Int

data Target = 
    NamedTarget VariableName
    | Register RegisterName
    | FrameVar (Offset, Size, VariableName)
    | ConstantTarget Int
    deriving (Show, Eq)

data Instructions = 
    PureAsm String
    | BeginFrame StackFrame (Maybe Label)
    | EndFrame StackFrame
    | Label Label
    | Move Target Target
    | Call Label [Target]
    | Compare Target Target
    | Jump Label (Maybe CompareType)
    deriving (Show, Eq)

type InstructionSet = [Instructions]

newtype BackEnd = BackEnd (InstructionSet -> String)
runBackend (BackEnd f) x = f x

target :: StackFrame -> VariableData -> Target
target _ (VariableData (_, VariableDeclaration (HLasm.Ast.Register(_, reg)))) = HLasm.Instructions.Register reg
target frame (VariableData (name, e)) = case findOffset frame name of
    Just x  -> FrameVar (x, size e, name)
    Nothing -> NamedTarget name
    where size (VariableDeclaration v) = valueSize v

findTarget :: StackFrame -> [VariableData] -> VariableName -> Target
findTarget frame xs name = target frame . fromJust . find (\(VariableData (n, _)) -> n == name) $ xs

valuableTarget :: (StackFrame, [VariableData]) -> HLValue -> Target
valuableTarget _ (IntegerValue v)        = ConstantTarget v
valuableTarget (sf, vd) (NameValue name) = findTarget sf vd name

loop :: Label -> Maybe (InstructionSet) -> Maybe (InstructionSet)
loop lbl i = fmap (\x -> [Label (lbl ++ "begin")] ++ x ++ [Label (lbl ++ "end")]) $ i

instructions :: Tree (HLElement, [VariableData], [LabelData], StackFrame) -> Maybe (InstructionSet)
instructions (Node ((InstructionSet         ), _, _, _) xs) = concatMapM instructions xs
instructions (Node ((VariableDeclaration val), _, _, _) _ ) = Just []
instructions (Node ((While lbl              ), _, _, _) xs) = loop lbl (concatMapM instructions xs)
instructions (Node ((DoWhile lbl            ), _, _, _) xs) = loop lbl (concatMapM instructions xs)
instructions (Node ((Break lbl              ), _, _, _) _ ) = Just [Jump (lbl ++ "end") Nothing]
instructions (Node ((HLasm.Ast.Call lbl ns  ), d, _, f) _ ) = Just [HLasm.Instructions.Call lbl (fmap (findTarget f d) ns)]
instructions (Node ((AssemblyCall str       ), _, _, _) _ ) = Just [PureAsm str]
instructions (Node ((Frame lbl              ), _, _, f) xs) =
    fmap (\x -> [BeginFrame f lbl] ++ x ++ [EndFrame f]) $ concatMapM instructions xs

instructions (Node ((Assignment name (NameValue val)),    d, _, f) _) = Just [Move (findTarget f d name) (findTarget f d val)]
instructions (Node ((Assignment name (IntegerValue val)), d, _, f) _) = Just [Move (findTarget f d name) (ConstantTarget val)]

instructions (Node ((If lbl), _, _, _) []) = Just []
instructions (Node ((If lbl), _, _, _) xs) =
    let (conds, bodies') = traverse (uncurry branch) (zip [1..] xs);
        bodies = fmap (concat) . sequence $ bodies'
     in fmap (\b -> conds ++ b ++ [Label (lbl ++ "end")]) bodies
    where condition pt lbl (Condition (left, cmp, right)) =
              [Compare (valuableTarget pt left) (valuableTarget pt right), Jump lbl (Just cmp)]
          wrapif lbl i = fmap (\b -> [Label (lbl ++ show i)] ++ b ++ [Jump (lbl ++ "end") Nothing])
          branch i (Node ((IfBranch (Just cond)), d, _, f) xs)  =
              (condition (f, d) (lbl ++ show i) cond, wrapif lbl i (concatMapM instructions xs))
          branch i (Node ((IfBranch Nothing),     _, _, _) xs)  =
              ([Jump (lbl ++ show i) Nothing], wrapif lbl i (concatMapM instructions xs))
          a = traverse (uncurry branch) (zip [1..] xs)
