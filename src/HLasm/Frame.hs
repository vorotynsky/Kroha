-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Frame
( VarFrame(..)
, FrameTree(..)
, frameSize
, buildFrame
, buildFrameTree
) where

import           Data.List
import           Data.Tree
import           HLasm.Ast

valueSize :: HLValuable -> Int
valueSize (Variable (_, t)) = size t
    where size (Type _ (Just s)) = s
          {- TODO: add support or refactor in future (on adding errors to compiler) -}
          size (Type _ Nothing)  = error "Unsupported types withoud specified size"
valueSize (Register (_, r)) = undefined

newtype VarFrame = VarFrame [(HLValuable, Int)]
    deriving (Show, Eq)

empty :: VarFrame
empty = VarFrame []

frameSize :: VarFrame -> Int
frameSize = foldr (+) 0 . fmap valueSize . (\(VarFrame xs) -> fmap fst xs)

buildFrame :: [HLValuable] -> VarFrame
buildFrame xs = VarFrame $ zip xs (fmap (foldl (+) 0) . inits . fmap valueSize $ xs)

type FrameTree = Tree (HLElement, VarFrame)

frameVars :: SyntaxTree -> [HLValuable]
frameVars (Node el@(Frame _) [])    = []
frameVars (Node el@(Frame _) (x:_)) = frameVars x
    where frameVars (Node (VariableDeclaration val@(Variable _)) xs) = [val] ++ (concatMap frameVars xs)
          frameVars (Node el@(Frame _) _) = []
          frameVars (Node _ xs) = concatMap frameVars xs

buildFrameTree :: SyntaxTree -> FrameTree
buildFrameTree t@(Node el@(HLasm.Ast.Frame _) xs) = Node (el, buildFrame . frameVars $ t) (fmap buildFrameTree xs)
buildFrameTree (Node el xs) = Node (el, empty) (fmap buildFrameTree xs)
