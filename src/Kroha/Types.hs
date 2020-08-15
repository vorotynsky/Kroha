module Kroha.Types where

import Data.Graph (Tree(..))
import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)
import Control.Monad (join)

import Kroha.Ast
import Kroha.Scope

sysregs = ["ip", "sp", "bp", "si", "di", "cs", "ds", "ss", "es", "fs"]

registerSize :: RegisterName -> Int
registerSize (n:"l")     | elem n "abcd"  = 8
registerSize (n:"h")     | elem n "abcd"  = 8
registerSize (n:"x")     | elem n "abcd"  = 16
registerSize ('e':n:"x") | elem n "abcd"  = 32
registerSize ('r':n:"x") | elem n "abcd"  = 64
registerSize (x)         | elem x sysregs = 16
registerSize ('e':x)     | elem x sysregs = 32
registerSize ('r':x)     | elem x sysregs = 64
registerSize _                            = 0

registerType reg = let rs = registerSize reg in if rs > 0 then Just . TypeName $ "int" ++ show rs else Nothing

typeSize :: Int -> TypeName -> Int
typeSize ptr (PointerType _)            = ptr
typeSize _   (TypeName ('i':'n':'t':x)) = read x
typeSize _   _                          = 0

getType :: ScopeLink -> Maybe TypeName
getType (ElementLink (VariableDeclaration (StackVariable    _ t)) _) = Just t
getType (ElementLink (VariableDeclaration (RegisterVariable _ r)) _) = registerType r
getType (DeclarationLink (GlobalVariable   _ t _) _)                 = Just t
getType (DeclarationLink (ConstantVariable _ t _) _)                 = Just t
getType (DeclarationLink (ManualVariable   _ t _) _)                 = Just t
getType _                                                            = Nothing

rvalType :: Scope -> RValue -> Maybe TypeName
rvalType  _ (RLiteral (IntegerLiteral 0 )) = Just . TypeName $ "int1"
rvalType  _ (RLiteral (IntegerLiteral x )) = Just . TypeName $ "int" ++ (show $ ceiling (logBase 2 (abs $ toEnum x + 1)))
rvalType  s (AsRValue (VariableLVal name)) = join . fmap getType $ lookup (VariableScope name) s
rvalType  _ (AsRValue (RegisterLVal reg )) = registerType reg

type TypeCast = (TypeName, TypeName)

makeTypeCast :: Scope -> (RValue, RValue) -> TypeCast
makeTypeCast scope values = bimap find find values
    where find x = fromJust . rvalType scope $ x -- scope is checked

casts :: FrameElement -> Scope -> [TypeCast]
casts (Instructions _)                  _ = []
casts (VariableDeclaration _)           _ = []
casts (If _ (Condition (a, _, b)) _ _)  s = fmap (makeTypeCast s) [(a, b)]
casts (Loop _ _)                        _ = []
casts (Break _)                         _ = []
casts (Call _ _)                        _ = [] -- todo: types for call
casts (Assignment lval rval)            s = fmap (makeTypeCast s) [(AsRValue lval, rval)]
casts (Inline _)                        _ = []

typeCasts :: Tree (ScopeLink, Scope) -> Tree [TypeCast]
typeCasts = let f ((RootProgramLink _  ), _)     = []
                f ((DeclarationLink _ _), _)     = []
                f ((ElementLink el _)   , scope) = casts el scope 
            in fmap f

resolve :: Int -> Tree [TypeCast] -> Either TypeCast (Tree [TypeCast])
resolve ptr = sequenceA . fmap sequenceA . (fmap . fmap) resolver
    where resolver tc = if lsize > 0 && rsize > 0 then Right tc else Left tc
              where (lsize, rsize) = let size = typeSize ptr in bimap size size tc
