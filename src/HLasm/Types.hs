-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Types where

import           Data.Maybe
import           Data.Tree
import           HLasm.Ast
import           HLasm.Scope

typeSuit :: Type -> Type -> Bool
typeSuit (Type lname _) (Type rname _) | (lname /= rname) = False
typeSuit (Type _ l) (Type _ r) = (fromMaybe maxBound l) >= (fromMaybe maxBound r)

registerSize :: RegisterName -> Int
registerSize (n:"l")     | elem n "abcd" = 8
registerSize (n:"h")     | elem n "abcd" = 8
registerSize (n:"x")     | elem n "abcd" = 16
registerSize ('e':n:"x") | elem n "abcd" = 32
registerSize ('r':n:"x") | elem n "abcd" = 64
registerSize _                           = 0

getType :: HLElement -> Type
getType (VariableDeclaration (Variable (_, t))) = t
getType (VariableDeclaration (Register (_, r))) = Type "int" (Just (registerSize r))

lookupType :: VariableName -> [VariableData] -> Maybe Type
lookupType name = fmap getType . lookup name . fmap (\(VariableData x) -> x)

literalType :: [VariableData] -> HLValue -> Maybe Type
literalType _ (IntegerValue x) = Just $ Type "int" (Just $ size x)
    where size = ceiling . (\x -> log (x + 1) / log 2) . toEnum
literalType s (NameValue name) = lookupType name s
literalType _ _ = undefined

astCheck :: HLElement -> [VariableData] -> Bool
astCheck (IfBranch (Just (Condition (left, _, right)))) xs = fromMaybe False $
    do leftType  <- literalType xs left
       rightType <- literalType xs right
       Just $ typeSuit leftType rightType
astCheck (Assignment left right) xs = fromMaybe False $
    do leftType  <- lookupType left xs
       rightType <- literalType xs right
       Just $ typeSuit leftType rightType
astCheck _ _ = True

typeCheck :: SemanticTree -> Maybe SemanticTree
typeCheck tree = traverse f tree
    where f x@(elem, vars, _) = if astCheck elem vars then Just x else Nothing

