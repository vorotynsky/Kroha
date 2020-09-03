module Kroha.Backends.Common (Backend(..), runBackend) where

import Data.Tree
import Kroha.Ast (Declaration(..))
import Kroha.Instructions (Instruction(Body), Section)
import Control.Monad (join)
import Data.Char (isSpace)
import Data.Semigroup (Min(Min, getMin))


data Backend = Backend 
    { instruction :: Instruction -> [String]
    , bodyWrap :: [String] -> [String]
    , indent :: String
    , section :: Section -> String -> String
    , declaration :: Declaration -> [String] -> String }


makeFix :: Backend -> Tree [Instruction] -> [String]
makeFix backend (Node i c) = join . fmap asmFix $ i
    where asmFix (Body _ i) = fmap ((++) (indent backend)) . bodyWrap backend $ makeFix backend (c !! i)
          asmFix i          = instruction backend i

unindentManual :: String -> [String]
unindentManual code = fmap (drop minIndent) lined
    where lined = lines code
          minIndent = getMin $ foldMap (Min . length . takeWhile isSpace) lined

backendDeclaration :: Backend -> Declaration -> Tree [Instruction] -> String
backendDeclaration b decl@(Frame _ frame)          ti = declaration b decl (makeFix b ti)
backendDeclaration b decl@(GlobalVariable   _ _ l) _  = declaration b decl []
backendDeclaration b decl@(ConstantVariable _ _ l) _  = declaration b decl []
backendDeclaration b decl@(ManualFrame _ c)        _  = declaration b decl (unindentManual c)
backendDeclaration b decl@(ManualVariable _ _ c)   _  = declaration b decl (unindentManual c)

runBackend :: Backend -> [(Section, Declaration, Tree [Instruction])] -> String
runBackend backend = join . fmap (mapper)
    where mapper (s, d, i) = section backend s (backendDeclaration backend d i)

