module Kroha.Stack where

import Data.Tree
import Data.List (mapAccumL)
import Data.Maybe (fromJust)

import Kroha.Ast
import Kroha.Types

type StackRange = (Int, Int) {-offset, size-}

stackVar :: TypeConfig -> FrameElement -> Int
stackVar tc (VariableDeclaration (StackVariable _ (PointerType _))) = snd $ types tc !! pointerType tc
stackVar tc (VariableDeclaration (StackVariable _ t@(TypeName _)))  = fromJust $ lookup t (types tc) 
stackVar _   _                                                      = 0

frame :: TypeConfig -> Tree FrameElement -> (Int, Tree StackRange)
frame ptr tree = mapAccumL f 0 tree
    where f acc el = let size = stackVar ptr el in (acc + size, (if size > 0 then acc + size else 0, size))

stackFrames :: TypeConfig -> Program -> [(Int, Tree StackRange)]
stackFrames ptr (Program declarations) = fmap mapper declarations
    where mapper (Frame _ f) = frame ptr (selector id f)
          mapper _           = (0, Node (0, 0) [])

stack :: TypeConfig -> Program -> Tree StackRange
stack ptr program = Node (0, 0) (fmap (Node (0, 0) . pure . snd) (stackFrames ptr program))
