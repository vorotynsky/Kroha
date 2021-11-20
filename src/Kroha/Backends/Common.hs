module Kroha.Backends.Common (Backend(..), runBackend) where

import Kroha.Syntax.Declarations (Declaration(..))
import Kroha.Types (TypeConfig)
import Kroha.Instructions (Instruction(Body), Section)

import Control.Monad (void)
import Data.Tree (Tree(..))
import Data.Char (isSpace)
import Data.Semigroup (Min(Min, getMin))


data Backend = Backend
    { typeConfig :: TypeConfig
    , instruction :: Instruction -> [String]
    , bodyWrap :: [String] -> [String]
    , indent :: String
    , section :: Section -> String -> String
    , declaration :: Declaration () -> [String] -> String }


makeFix :: Backend -> Tree [Instruction] -> [String]
makeFix backend (Node i c) = i >>= asmFix
    where asmFix (Body _ i) = fmap (indent backend ++) . bodyWrap backend $ makeFix backend (c !! i)
          asmFix i          = instruction backend i

unindentManual :: String -> [String]
unindentManual code = fmap (drop minIndent) lined
    where lined = let (h:t) = (\l -> if null l then [""] else l) $ lines code in if null h then t else h:t
          filterEmpty = filter (not . all isSpace)
          minIndent = getMin . foldMap (Min . length . takeWhile isSpace) . filterEmpty $ lined

backendDeclaration :: Backend -> Declaration () -> Tree [Instruction] -> String
backendDeclaration b decl@(Frame {})              ti = declaration b decl (makeFix b ti)
backendDeclaration b decl@(GlobalVariable   {})    _  = declaration b decl []
backendDeclaration b decl@(ConstantVariable {})    _  = declaration b decl []
backendDeclaration b decl@(ManualFrame _ c _)      _  = declaration b decl (unindentManual c)
backendDeclaration b decl@(ManualVariable _ _ c _) _  = declaration b decl (unindentManual c)

runBackend :: Backend -> [(Section, Declaration d, Tree [Instruction])] -> String
runBackend backend = (>>= mapper)
    where mapper (s, d, i) = section backend s (backendDeclaration backend (void d) i)

