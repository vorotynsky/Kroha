module Kroha.Stack where

import Control.Comonad (extract, duplicate, ($>))
import Data.List (mapAccumL)
import Data.Maybe (fromJust)

import Kroha.Syntax.Syntax
import Kroha.Types

type StackRange = (Int, Int) {-offset, size-}

stackVar :: TypeConfig -> FrameElement d -> Int
stackVar tc (VariableDeclaration (StackVariable _ (PointerType _)) _) = snd $ types tc !! pointerType tc
stackVar tc (VariableDeclaration (StackVariable _ t@(TypeName _))  _) = fromJust $ lookup t (types tc)
stackVar _   _                                                        = 0

frame :: TypeConfig -> FrameElement d -> FrameElement (Int, StackRange)
frame ptr = snd . mapAccumL f 0 . duplicate
    where f acc el = let size = stackVar ptr el in (acc + size, (acc, (if size > 0 then acc + size else 0, size)))

stackFrames :: TypeConfig -> Program d -> Program (Int, StackRange)
stackFrames ptr (Program declarations _) = Program (fmap mapper declarations) (0, (0, 0))
    where mapper (Frame l f _) = let f' = frame ptr f in Frame l f' (extract f')
          mapper d             = d $> (0, (0, 0))

stack :: TypeConfig -> Program d -> Program StackRange
stack ptr program = snd <$> stackFrames ptr program
