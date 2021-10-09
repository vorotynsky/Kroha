-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

{-# LANGUAGE ImplicitParams #-}

module Kroha.Types where

import Data.Graph (Tree(..), Graph(..), path)
import Data.Bifunctor (bimap)
import Control.Monad (join)
import Data.List.Extra (elemIndex)
import Data.Either.Extra (maybeToEither)

import Kroha.Ast
import Kroha.Scope
import Kroha.Errors

type TypeId = Int

data TypeConfig = TypeConfig 
    { types       :: [(TypeName, Int)]
    , pointerType :: TypeId
    , registers   :: [(RegisterName, TypeId)]
    , typeCasts   :: Graph
    , literalType :: Literal -> Result TypeId }

types' tc (PointerType _) = Right (pointerType tc)
types' tc t               = maybeToEither (UnknownType t) (t `elemIndex` (fst . unzip . types $ tc))
    
declType :: Declaration d -> TypeName
declType (GlobalVariable   _ t _ _) = t
declType (ConstantVariable _ t _ _) = t
declType (ManualVariable   _ t _ _) = t


getType :: (?tc :: TypeConfig) => ScopeLink -> Result TypeId
getType (ElementLink (VariableDeclaration (RegisterVariable _ r) _)) = firstE UnknownRegister $ findEither r (registers ?tc)
getType (ElementLink (VariableDeclaration (StackVariable    _ t) _)) = types' ?tc t
getType (DeclarationLink declaration)                                = types' ?tc (declType declaration)
getType _                                                            = Left (error "unexpected type error")


rvalType :: (?tc :: TypeConfig) => Scope -> RValue -> Result TypeId
rvalType _ (RLiteral literal) = literalType ?tc literal
rvalType s (AsRValue (VariableLVal name)) = maybeToEither (VariableNotFound name) (lookup (VariableScope name) s) >>= getType
rvalType _ (AsRValue (RegisterLVal reg )) = maybeToEither (UnknownRegister reg) $ lookup reg (registers ?tc)


type TypeCast = (TypeId, TypeId)

extractM :: Monad m => (m a, m b) -> m (a, b)
extractM (a, b) = do x <- a; y <- b; return (x, y)

makeTypeCast :: (?tc :: TypeConfig) => Scope -> (RValue, RValue) -> Result TypeCast
makeTypeCast scope values = extractM $ bimap find find values
    where find x = rvalType scope $ x

casts :: (?tc :: TypeConfig) => FrameElement d -> Scope -> [Result TypeCast]
casts (Instructions _ _)                  _ = []
casts (VariableDeclaration _ _)           _ = []
casts (If _ (Condition (a, _, b)) _ _ _)  s = fmap (makeTypeCast s) [(a, b)]
casts (Loop _ _ _)                        _ = []
casts (Break _ _)                         _ = []
casts (Call _ _ _)                        _ = [] -- todo: types for call
casts (Assignment lval rval _)            s = fmap (makeTypeCast s) [(AsRValue lval, rval)]
casts (Inline _ _)                        _ = []

typeCastsTree :: TypeConfig -> Program (ScopeLink, Scope) -> Result (Program [TypeCast])
typeCastsTree tc = let f (RootProgramLink _, _)    = []
                       f (DeclarationLink _, _)  = []
                       f (ElementLink el, scope) = let ?tc = tc in casts el scope
                    in sequenceErrors (JoinedError . join) . fmap (partitionErrors . f)

resolve :: TypeConfig -> Program [TypeCast] -> Result (Program [TypeCast])
resolve config = firstE typeName . traverse sequenceA . (fmap . fmap) (resolveCast (typeCasts config))
    where resolveCast g c@(f, t) = if path g f t then Right c else Left c
          typeName (t1, t2) = let name typeId = fst $ types config !! typeId in TypeCastError (name t2) (name t1)
