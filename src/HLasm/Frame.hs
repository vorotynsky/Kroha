-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Frame
( VarFrame(..)
, stackVarSize
, frameSize
, buildFrame
, buildFrameTree
, StackFrame(..)
, buildStackFrames
, findOffset 
) where

import           Data.List
import           Data.Tree
import           HLasm.Ast
import           HLasm.Scope hiding (Scope(Root))

size (Type _ (Just s)) = s
{- TODO: add support or refactor in future (on adding errors to compiler) -}
size (Type _ Nothing)  = error "Unsupported types without specified size"

stackVarSize :: HLElement -> Int
stackVarSize (VariableDeclaration  _ t  ) = size t
stackVarSize (GlobalVarDeclaration _ t _) = size t
stackVarSize (ConstVarDeclaration  _ t _) = size t
stackVarSize (RegisterDeclaration  _ r  ) = 0

newtype VarFrame = VarFrame [(HLElement, Int, Int)]
    deriving (Show, Eq) 

empty :: VarFrame
empty = VarFrame []

frameSize :: VarFrame -> Int
frameSize = foldr (+) 0 . fmap stackVarSize . (\(VarFrame xs) -> fmap (\(x,_,_) -> x) xs)

buildFrame :: [HLElement] -> VarFrame
buildFrame xs = VarFrame $ zipWith (\v o -> (v, o, stackVarSize v)) xs (fmap (foldl (+) 0) . inits . fmap stackVarSize $ xs)

frameVars :: SyntaxTree -> [HLElement]
frameVars (Node el@(Frame _) [])    = []
frameVars (Node el@(Frame _) (x:_)) = frameVars x
    where frameVars (Node val xs) | isVariable val = [val] ++ (concatMap frameVars xs)
          frameVars (Node el@(Frame _) _) = []
          frameVars (Node _ xs) = concatMap frameVars xs

buildFrameTree :: SyntaxTree -> Tree (HLElement, VarFrame)
buildFrameTree t@(Node el@(HLasm.Ast.Frame _) xs) = Node (el, buildFrame . frameVars $ t) (fmap buildFrameTree xs)
buildFrameTree (Node el xs) = Node (el, empty) (fmap buildFrameTree xs)

data StackFrame = 
    Root 
    | Fluent { parentFrame :: StackFrame }
    | StackFrame 
        { parentFrame :: StackFrame
        , variables   :: VarFrame }
    deriving (Show, Eq)

buildStackFrames :: StackFrame -> SyntaxTree -> Tree (HLElement, StackFrame)
buildStackFrames parent tree@(Node el@(Frame _) xs) = Node (el, frame) (fmap (buildStackFrames frame) xs)
    where frame = StackFrame parent . buildFrame $ frameVars tree 
buildStackFrames parent (Node el xs) = Node (el, Fluent parent) (fmap (buildStackFrames parent) xs)

findOffset :: StackFrame -> VariableName -> (Maybe Int)
findOffset Root _                        = Nothing
findOffset (Fluent parent) name          = findOffset parent name
findOffset (StackFrame parent vars) name 
    | (any predicate list) = fmap (\(_, o, _) -> o) . find predicate $ list
    | otherwise = fmap (+ frameSize vars) $ findOffset parent name -- -> change last commit
    where list = (\(VarFrame xs) -> xs) vars
          predicate (el, _, _) | isVariable el && (variableName el) == name = True
          predicate _ = False
