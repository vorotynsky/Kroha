module Kroha.Stack where

import Data.Tree
import Data.List (mapAccumL)
import Data.Maybe (fromJust)

import Kroha.Ast
import Kroha.Types

type StackRange = (Int, Int) {-offset, size-}

stackVar :: TypeConfig -> FrameElement d -> Int
stackVar tc (VariableDeclaration (StackVariable _ (PointerType _)) _) = snd $ types tc !! pointerType tc
stackVar tc (VariableDeclaration (StackVariable _ t@(TypeName _))  _) = fromJust $ lookup t (types tc)
stackVar _   _                                                        = 0

frame :: TypeConfig -> Tree (FrameElement d) -> (Int, Tree StackRange)
frame ptr = mapAccumL f 0
    where f acc el = let size = stackVar ptr el in (acc + size, (if size > 0 then acc + size else 0, size))

stackFrames :: TypeConfig -> Program d -> [(Int, Tree StackRange)]
stackFrames ptr (Program declarations _) = fmap mapper declarations
    where mapper (Frame _ f _) = frame ptr (selector id f)
          mapper _             = (0, Node (0, 0) [])

stack :: TypeConfig -> Program d -> Tree StackRange
stack ptr program = Node (0, 0) (fmap (Node (0, 0) . pure . snd) (stackFrames ptr program))
