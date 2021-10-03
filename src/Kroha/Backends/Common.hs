module Kroha.Backends.Common (Backend(..), runBackend) where

import Kroha.Ast (Declaration(..))
import Kroha.Types (TypeConfig)
import Kroha.Instructions (Instruction(Body), Section)

import Control.Monad (join, void)
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
makeFix backend (Node i c) = join . fmap asmFix $ i
    where asmFix (Body _ i) = fmap ((++) (indent backend)) . bodyWrap backend $ makeFix backend (c !! i)
          asmFix i          = instruction backend i

unindentManual :: String -> [String]
unindentManual code = fmap (drop minIndent) lined
    where lined = let (h:t) = (\l -> if null l then [""] else l) $ lines code in if null h then t else h:t
          filterEmpty = filter (not . null . filter (not . isSpace))
          minIndent = getMin . foldMap (Min . length . takeWhile isSpace) . filterEmpty $ lined

backendDeclaration :: Backend -> Declaration () -> Tree [Instruction] -> String
backendDeclaration b decl@(Frame _ frame _)          ti = declaration b decl (makeFix b ti)
backendDeclaration b decl@(GlobalVariable   _ _ l _) _  = declaration b decl []
backendDeclaration b decl@(ConstantVariable _ _ l _) _  = declaration b decl []
backendDeclaration b decl@(ManualFrame _ c _)        _  = declaration b decl (unindentManual c)
backendDeclaration b decl@(ManualVariable _ _ c _)   _  = declaration b decl (unindentManual c)

runBackend :: Backend -> [(Section, Declaration d, Tree [Instruction])] -> String
runBackend backend = join . fmap (mapper)
    where mapper (s, d, i) = section backend s (backendDeclaration backend (void d) i)

