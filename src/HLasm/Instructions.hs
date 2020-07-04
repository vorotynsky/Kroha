-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Instructions
( Offset(..), Target(..), InstructionSet(..)
, Instructions(..), Variable(..)
, Section(..), ObjProgram(..)
, BackEnd(..)
, runBackend
, program
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

data Variable = Variable VariableName Type RValue

data Section =
    Text InstructionSet
    | Data [Variable]
    | Constants [Variable]

newtype ObjProgram = ObjProgram [Section]

newtype BackEnd = BackEnd (ObjProgram -> Result String)
runBackend (BackEnd f) x = f x


target :: StackFrame -> VariableData -> Target
target _ (VariableData (_, (RegisterDeclaration _ reg))) = HLasm.Instructions.Register reg
target frame (VariableData (name, e)) = case findOffset frame name of
    Just x  -> FrameVar (x, stackVarSize e, name)
    Nothing -> NamedTarget name

findTarget :: StackFrame -> [VariableData] -> VariableName -> Target --  was a lot of checks, target garanteed be here.
findTarget frame xs name = target frame . fromJust . find (\(VariableData (n, _)) -> n == name) $ xs

valuableTarget :: (StackFrame, [VariableData]) -> RValue -> Target
valuableTarget _        (IntegerValue v)                = ConstantTarget v
valuableTarget (sf, vd) (LeftValue(NameValue name))     = findTarget sf vd name
valuableTarget (sf, vd) (LeftValue(RegisterValue name)) = Register name

loop :: Label -> Result (InstructionSet) -> Result (InstructionSet)
loop lbl i = let begin = lbl ++ "begin" in fmap (\x -> [Label begin] ++ x ++ [Jump begin Nothing, Label (lbl ++ "end")]) $ i

isEmptyInstruction :: HLElement -> Bool
isEmptyInstruction (FakeVariable _)          = True
isEmptyInstruction (FakeFrame _)             = True
isEmptyInstruction (VariableDeclaration _ _) = True
isEmptyInstruction (RegisterDeclaration _ _) = True
isEmptyInstruction _                         = False

rval2target :: RValue -> StackFrame -> [VariableData] -> Target
rval2target (IntegerValue val)               _ _ = ConstantTarget val
rval2target (LeftValue (RegisterValue name)) _ _ = Register name
rval2target (LeftValue (NameValue     name)) s d = findTarget s d name

instructions :: Tree (HLElement, [VariableData], [LabelData], StackFrame) -> Result (InstructionSet)
instructions (Node (el, _, _, _) _) | isEmptyInstruction el = Right []
instructions (Node ((InstructionSet         ), _, _, _) xs) = concatMapM instructions xs
instructions (Node ((While lbl              ), _, _, _) xs) = loop lbl (concatMapM instructions xs)
instructions (Node ((DoWhile lbl            ), _, _, _) xs) = loop lbl (concatMapM instructions xs)
instructions (Node ((Break lbl              ), _, _, _) _ ) = Right [Jump (lbl ++ "end") Nothing]
instructions (Node ((AssemblyCall str       ), _, _, _) _ ) = Right [PureAsm str]
instructions (Node ((Frame lbl              ), _, _, f) xs) =
    (\body -> [BeginFrame f lbl] ++ body ++ [EndFrame f lbl]) <$> concatMapM instructions xs

instructions (Node ((GlobalVarDeclaration n _ _), _, _, _) _ ) = Left (GlobalVariableInFrame n)
instructions (Node ((ConstVarDeclaration n _ _), _, _, _) _ )  = Left (GlobalVariableInFrame n)

instructions (Node ((Assignment left right), d, _, f) _) 
    = Right [Move (rval2target (LeftValue left) f d) (rval2target right f d)]

instructions (Node ((HLasm.Ast.Call lbl ns  ), d, _, f) _ ) = 
    Right [HLasm.Instructions.Call lbl (fmap (\n -> rval2target n f d) ns) size]
    where size = foldl (+) 0 . fmap (\(VariableData (_, d)) -> stackVarSize d) $ d

instructions (Node ((If lbl), _, _, _) []) = Right []
instructions (Node ((If lbl), _, _, _) xs) =
    do (conds, bodies') <- Right $ traverse (uncurry branch) (zip [1..] xs)
       bodies           <- fmap (concat) . sequence $ bodies'
       Right $ conds ++ [Jump (lbl ++ "end") Nothing] ++ bodies ++ [Label (lbl ++ "end")]
    where condition lbl pt (Condition (left, cmp, right)) =
              let find = valuableTarget pt in [Compare (find left) (find right), Jump lbl (Just cmp)]
          wrapif i = fmap (\b -> [Label (lbl ++ show i)] ++ b ++ [Jump (lbl ++ "end") Nothing]) . concatMapM instructions
          branch i (Node ((IfBranch (Just cond)), d, _, f) xs) = (condition (lbl ++ show i) (f, d) cond, wrapif i xs)
          branch i (Node ((IfBranch Nothing),     _, _, _) xs) = ([Jump     (lbl ++ show i)    Nothing], wrapif i xs)


dataFilter  (Node ((GlobalVarDeclaration n t v), _, _, _) _) = Just $ Variable n t v
dataFilter  _                                                = Nothing
constFilter (Node ((ConstVarDeclaration n t v), _, _, _) _)  = Just $ Variable n t v
constFilter _                                                = Nothing

varSection ctor f xs = Right . ctor . fmap fromJust . filter isJust . fmap f $ xs

filterF :: [(a -> Maybe b)] -> (a -> Bool)
filterF fs = foldl or (const True) . fmap ((.) (not . isJust)) $ fs
    where or f g x = f x && g x

program :: Tree (HLElement, [VariableData], [LabelData], StackFrame) -> Result ObjProgram
program (Node ((Program), _, _, _) xs) = 
    do text   <- concatMapM instructions . filter (filterF [dataFilter, constFilter]) $ xs
       dat    <- varSection Data      dataFilter  xs
       const  <- varSection Constants constFilter xs
       Right $ ObjProgram [(Text text), dat, const]
program _ = undefined
