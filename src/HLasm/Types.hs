-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Types where

import           Data.Maybe
import           Data.Tree
import           Data.Bifunctor
import           HLasm.Ast
import           HLasm.Scope
import           HLasm.Error

typeSuit :: Type -> Type -> Bool
typeSuit (Type "any" _) _ = True
typeSuit _ (Type "any" _) = True
typeSuit (Type lname _) (Type rname _) | (lname /= rname) = False
typeSuit (Type _ l) (Type _ r) = (fromMaybe maxBound l) >= (fromMaybe maxBound r)

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

registerType = Type "int" . Just . registerSize

getType :: HLElement -> Type
getType (VariableDeclaration _ t)    = t
getType (ConstVarDeclaration _ t _)  = t
getType (GlobalVarDeclaration _ t _) = t
getType (RegisterDeclaration _ r)    = Type "int" (Just (registerSize r))
getType (FakeVariable _)             = Type "any" Nothing

getSize :: Type -> Int
getSize (Type "int" (Just size)) = size
getSize _                        = 0

lookupType :: VariableName -> [VariableData] -> Maybe Type
lookupType name = fmap getType . lookup name . fmap (\(VariableData x) -> x)

literalType :: [VariableData] -> RValue -> Maybe Type
literalType _ (IntegerValue x) = Just $ Type "int" (Just $ size x)
    where size = ceiling . (\x -> log (x + 1) / log 2) . toEnum
literalType s (LeftValue (NameValue     name)) = lookupType name s
literalType s (LeftValue (RegisterValue name)) = Just $ registerType name

err a b = maybe (Left (a, b)) Right

astCheck :: HLElement -> [VariableData] -> Either (RValue, RValue) ()
astCheck (IfBranch (Just (Condition (left, _, right)))) xs =
    do leftType  <- err left right $ literalType xs left
       rightType <- err left right $ literalType xs right
       if typeSuit leftType rightType then Right () else err left right Nothing
astCheck (Assignment left right) xs =
    let error = err (LeftValue left) right in
    do leftType  <- error $ literalType xs (LeftValue left)
       rightType <- error $ literalType xs right
       if typeSuit leftType rightType then Right () else error Nothing
astCheck _ _ = Right ()

typeCheck :: SemanticTree -> Either Error SemanticTree
typeCheck tree = traverse f tree
    where f x@(elem, vars, _) = bimap IncompatibleTypes (const x) $ astCheck elem vars
