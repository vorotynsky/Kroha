module Kroha.Stack where

import Data.Tree
import Data.List (mapAccumL)

import Kroha.Ast
import Kroha.Scope
import Kroha.Types

stackVar :: PointerSize -> FrameElement -> Int
stackVar ptr (VariableDeclaration (StackVariable _ t)) = typeSize ptr t
stackVar _   _                                         = 0

frame :: PointerSize -> Tree FrameElement -> (Int, Tree Int)
frame ptr tree = mapAccumL f 0 tree
    where f acc el = let size = stackVar ptr el in (acc + size, if size > 0 then acc + size else 0)

stackFrames :: PointerSize -> Program -> [(Int, Tree Int)]
stackFrames ptr (Program declarations) = fmap mapper declarations
    where mapper (Frame _ f) = frame ptr (selector id f)
          mapper _           = (0, Node 0 [])
