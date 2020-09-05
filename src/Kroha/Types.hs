{-# LANGUAGE ImplicitParams #-}

module Kroha.Types where

import Data.Graph (Tree(..), Graph(..), path)
import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)
import Control.Monad (join)

import Kroha.Ast
import Kroha.Scope
import Data.List.Extra (elemIndex)

type TypeId = Int

data TypeConfig = TypeConfig 
    { types       :: [(TypeName, Int)]
    , pointerType :: TypeId
    , registers   :: [(RegisterName, TypeId)]
    , typeCasts   :: Graph
    , literalType :: Literal -> Maybe TypeId }

types' tc (PointerType _) = Just (pointerType tc)
types' tc t               = t `elemIndex` (fst . unzip . types $ tc)

getType :: (?tc :: TypeConfig) => ScopeLink -> Maybe TypeId
getType (ElementLink (VariableDeclaration (RegisterVariable _ r)) _) = lookup r (registers ?tc)
getType (ElementLink (VariableDeclaration (StackVariable    _ t)) _) = types' ?tc t
getType (DeclarationLink (GlobalVariable   _ t _) _)                 = types' ?tc t
getType (DeclarationLink (ConstantVariable _ t _) _)                 = types' ?tc t
getType (DeclarationLink (ManualVariable   _ t _) _)                 = types' ?tc t
getType _                                                            = Nothing


rvalType :: (?tc :: TypeConfig) => Scope -> RValue -> Maybe TypeId
rvalType _ (RLiteral literal) = literalType ?tc literal
rvalType s (AsRValue (VariableLVal name)) = lookup (VariableScope name) s >>= getType
rvalType _ (AsRValue (RegisterLVal reg )) = lookup reg (registers ?tc)

type TypeCast = (TypeId, TypeId)

makeTypeCast :: (?tc :: TypeConfig) => Scope -> (RValue, RValue) -> TypeCast
makeTypeCast scope values = bimap find find values
    where find x = fromJust . rvalType scope $ x -- scope is checked

casts :: (?tc :: TypeConfig) => FrameElement -> Scope -> [TypeCast]
casts (Instructions _)                  _ = []
casts (VariableDeclaration _)           _ = []
casts (If _ (Condition (a, _, b)) _ _)  s = fmap (makeTypeCast s) [(a, b)]
casts (Loop _ _)                        _ = []
casts (Break _)                         _ = []
casts (Call _ _)                        _ = [] -- todo: types for call
casts (Assignment lval rval)            s = fmap (makeTypeCast s) [(AsRValue lval, rval)]
casts (Inline _)                        _ = []

typeCastsTree :: TypeConfig -> Tree (ScopeLink, Scope) -> Tree [TypeCast]
typeCastsTree tc = let f ((RootProgramLink _  ), _)     = []
                       f ((DeclarationLink _ _), _)     = []
                       f ((ElementLink el _)   , scope) = let ?tc = tc in casts el scope 
                   in fmap f

resolve :: TypeConfig -> Tree [TypeCast] -> Either TypeCast (Tree [TypeCast])
resolve config = sequenceA . fmap sequenceA . (fmap . fmap) (resolveCast (typeCasts config))
    where resolveCast g c@(f, t) = if path g f t then Right c else Left c
