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
import           HLasm.Error

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
    | EndFrame StackFrame (Maybe Label)
    | Label Label
    | Move Target Target
    | Call Label [Target] Int
    | Compare Target Target
    | Jump Label (Maybe CompareType)
    deriving (Show, Eq)

type InstructionSet = [Instructions]

newtype BackEnd = BackEnd (InstructionSet -> Result String)
runBackend (BackEnd f) x = f x


target :: StackFrame -> VariableData -> Target
target _ (VariableData (_, VariableDeclaration (HLasm.Ast.Register(_, reg)))) = HLasm.Instructions.Register reg
target frame (VariableData (name, e)) = case findOffset frame name of
    Just x  -> FrameVar (x, size e, name)
    Nothing -> NamedTarget name
    where size (VariableDeclaration v) = valueSize v

findTarget :: StackFrame -> [VariableData] -> VariableName -> Target --  was a lot of checks, target garanteed be here.
findTarget frame xs name = target frame . fromJust . find (\(VariableData (n, _)) -> n == name) $ xs

valuableTarget :: (StackFrame, [VariableData]) -> HLValue -> Target
valuableTarget _        (IntegerValue v) = ConstantTarget v
valuableTarget (sf, vd) (NameValue name) = findTarget sf vd name

loop :: Label -> Result (InstructionSet) -> Result (InstructionSet)
loop lbl i = let begin = lbl ++ "begin" in fmap (\x -> [Label begin] ++ x ++ [Jump begin Nothing, Label (lbl ++ "end")]) $ i

instructions :: Tree (HLElement, [VariableData], [LabelData], StackFrame) -> Result (InstructionSet)
instructions (Node ((InstructionSet         ), _, _, _) xs) = concatMapM instructions xs
instructions (Node ((VariableDeclaration val), _, _, _) _ ) = Right []
instructions (Node ((While lbl              ), _, _, _) xs) = loop lbl (concatMapM instructions xs)
instructions (Node ((DoWhile lbl            ), _, _, _) xs) = loop lbl (concatMapM instructions xs)
instructions (Node ((Break lbl              ), _, _, _) _ ) = Right [Jump (lbl ++ "end") Nothing]
instructions (Node ((AssemblyCall str       ), _, _, _) _ ) = Right [PureAsm str]
instructions (Node ((Frame lbl              ), _, _, f) xs) =
    (\body -> [BeginFrame f lbl] ++ body ++ [EndFrame f lbl]) <$> concatMapM instructions xs

instructions (Node ((Assignment name (NameValue val)),    d, _, f) _) = Right [Move (findTarget f d name) (findTarget f d val)]
instructions (Node ((Assignment name (IntegerValue val)), d, _, f) _) = Right [Move (findTarget f d name) (ConstantTarget val)]

instructions (Node ((HLasm.Ast.Call lbl ns  ), d, _, f) _ ) = 
    Right [HLasm.Instructions.Call lbl (fmap (findTarget f d) ns) size]
    where size = foldl (+) 0 . fmap (\(VariableData (_, (VariableDeclaration d))) -> valueSize d) $ d 

instructions (Node ((If lbl), _, _, _) []) = Right []
instructions (Node ((If lbl), _, _, _) xs) =
    do (conds, bodies') <- Right $ traverse (uncurry branch) (zip [1..] xs)
       bodies           <- fmap (concat) . sequence $ bodies'
       Right $ conds ++ bodies ++ [Label (lbl ++ "end")]

    where condition lbl pt (Condition (left, cmp, right)) =
              let find = valuableTarget pt in [Compare (find left) (find right), Jump lbl (Just cmp)]
          wrapif i = fmap (\b -> [Label (lbl ++ show i)] ++ b ++ [Jump (lbl ++ "end") Nothing]) . concatMapM instructions
          branch i (Node ((IfBranch (Just cond)), d, _, f) xs) = (condition (lbl ++ show i) (f, d) cond, wrapif i xs)
          branch i (Node ((IfBranch Nothing),     _, _, _) xs) = ([Jump     (lbl ++ show i)    Nothing], wrapif i xs)
