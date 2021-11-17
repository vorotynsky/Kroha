-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

module Kroha.Types where

import Data.Graph (Graph, path)
import Data.Bifunctor (bimap)
import Control.Monad (join)
import Data.List.Extra (elemIndex)
import Data.Either.Extra (maybeToEither)

import Kroha.Syntax.Syntax
import Kroha.Scope
import Kroha.Errors

type TypeId = Int

data TypeConfig = TypeConfig
    { types       :: [(TypeName, Int)]
    , pointerType :: TypeId
    , registers   :: [(RegisterName, TypeId)]
    , typeCasts   :: Graph
    , literalType :: Literal -> Result TypeId }

types' _ tc (PointerType _) = Right (pointerType tc)
types' nid tc t               = maybeToEither (UnknownType t nid) (t `elemIndex` (fst . unzip . types $ tc))

declType :: Declaration d -> TypeName
declType (GlobalVariable   _ t _ _) = t
declType (ConstantVariable _ t _ _) = t
declType (ManualVariable   _ t _ _) = t
declType (Frame              l _ _) = error $ "[Exception]: Unexpected declaration to extract type. \tError location: Frame       (" ++ l ++ ")"
declType (ManualFrame        l _ _) = error $ "[Exception]: Unexpected declaration to extract type. \tError location: ManualFrame (" ++ l ++ ")" 


getType :: (?tc :: TypeConfig) => ScopeLink -> Result TypeId
getType (ElementLink (VariableDeclaration (RegisterVariable _ r) d)) = firstE (`UnknownRegister` d) $ findEither r (registers ?tc)
getType (ElementLink (VariableDeclaration (StackVariable    _ t) d)) = types' d ?tc t
getType (DeclarationLink declaration)                                = types' (getDeclData declaration) ?tc (declType declaration)
getType _                                                            = Left (error "unexpected type error")

rvalType :: (?tc :: TypeConfig) => Scope -> RValue -> NodeId -> Result TypeId
rvalType _ (RLiteral literal)             nid = literalType ?tc literal
rvalType s (AsRValue (VariableLVal name)) nid = maybeToEither (VariableNotFound name nid) (lookup (VariableScope name) s) >>= getType
rvalType _ (AsRValue (RegisterLVal reg )) nid = maybeToEither (UnknownRegister reg nid) $ lookup reg (registers ?tc)


type TypeCast = (TypeId, TypeId)

extractE (a, b) = (\[a', b'] -> (a', b')) <$> sequenceErrors JoinedError [a, b]

makeTypeCast :: (?tc :: TypeConfig) => Scope -> NodeId -> (RValue, RValue) -> Result TypeCast
makeTypeCast scope nid values = extractE $ bimap find find values
    where find x = rvalType scope x nid

casts :: (?tc :: TypeConfig) => FrameElement NodeId -> Scope -> [Result TypeCast]
casts (Instructions _ _)                  _ = []
casts (VariableDeclaration _ _)           _ = []
casts (If _ (Condition (a, _, b)) _ _ d)  s = fmap (makeTypeCast s d) [(a, b)]
casts (Loop _ _ _)                        _ = []
casts (Break _ _)                         _ = []
casts (Call _ _ _)                        _ = [] -- todo: types for call
casts (Assignment lval rval d)            s = fmap (makeTypeCast s d) [(AsRValue lval, rval)]
casts (Inline _ _)                        _ = []

typeCastsTree :: TypeConfig -> Program (ScopeLink, Scope) -> Result (Program [TypeCast])
typeCastsTree tc = let f (RootProgramLink _, _)  = []
                       f (DeclarationLink _, _)  = []
                       f (ElementLink el, scope) = let ?tc = tc in casts el scope
                    in sequenceErrors (JoinedError . join) . fmap (partitionErrors . f)

resolve :: TypeConfig -> Program (NodeId, [TypeCast]) -> Result (Program [TypeCast])
resolve config = processError (map (resolveCast (typeCasts config)) . append)
    where resolveCast g (nid, c@(f, t)) = if path g f t then Right c else Left (nid, c)
          typeName (nid, (t1, t2)) = let name typeId = fst $ types config !! typeId in TypeCastError (name t2) (name t1) nid
          append (a, l) = fmap (a,) l
          processError f = sequenceErrors JoinedError . fmap (sequenceErrors (JoinedError . map typeName) . f)
