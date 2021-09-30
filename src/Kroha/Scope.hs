-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

{-# LANGUAGE TupleSections #-}

module Kroha.Scope where

import           Control.Comonad   (extract)
import           Control.Monad     (void)
import           Control.Monad.Zip (munzip, mzip, mzipWith)
import           Data.Bifunctor    (first)
import           Data.Foldable     (find)
import           Data.Maybe        (fromJust, mapMaybe)
import           Data.Tree         (Tree (..))

import           Kroha.Ast
import           Kroha.Errors


data ScopeEffect
    = VariableScope VariableName
    | LabelScope Label
    deriving (Eq, Show)

type RequestFromScope = ScopeEffect
type PushToScope = ScopeEffect

requestVars :: [RValue] -> [RequestFromScope]
requestVars = mapMaybe (fmap VariableScope . rvalueScope)
    where rvalueScope (AsRValue (VariableLVal name)) = Just name
          rvalueScope _                              = Nothing

scope :: Selector d ([PushToScope], [RequestFromScope])
scope (Instructions _ _)                                = ([]                    , [])
scope (VariableDeclaration (StackVariable name _) _)    = ([VariableScope name ] , [])
scope (VariableDeclaration (RegisterVariable name _) _) = ([VariableScope name ] , [])
scope (If label (Condition (a, _, b)) _ _ _)            = ([LabelScope    label] , requestVars [a, b])
scope (Loop label _ _)                                  = ([LabelScope    label] , [])
scope (Break label _)                                   = ([]                    , LabelScope label:[])
scope (Call label args _)                               = ([]                    , LabelScope label : requestVars args)
scope (Assignment lval rval _)                          = ([]                    , requestVars [AsRValue lval, rval])
scope (Inline _ _)                                      = ([]                    , [])

dscope :: Declaration d -> PushToScope
dscope (Frame label _ _)             = LabelScope    label
dscope (GlobalVariable name _ _ _)   = VariableScope name
dscope (ConstantVariable name _ _ _) = VariableScope name
dscope (ManualFrame label _ _)       = LabelScope    label
dscope (ManualVariable name _ _ _)   = VariableScope name

dscope' d = ([dscope d], [] :: [RequestFromScope])

type Scope = [(PushToScope, ScopeLink)]

toRight _ (Just x) = Right x
toRight x (Nothing) = Left x

findEither k = toRight k . lookup k

data ScopeLink
    = ElementLink (FrameElement ()) NodeId
    | DeclarationLink (Declaration ()) NodeId
    | RootProgramLink NodeId
    deriving (Show)

localScope :: Program d -> Tree ([PushToScope], [RequestFromScope])
localScope program = Node ([], []) (selectorProg dscope' scope program)

linksTree :: Program d -> Tree ScopeLink
linksTree program = mzipWith id (Node RootProgramLink (selectorProg DeclarationLink ElementLink (void program))) (progId program)

scopeTree :: Scope -> Tree ([PushToScope], ScopeLink) -> Tree Scope
scopeTree parent (Node effect childs) = Node (eZip effect ++ parent) childScope
    where folder acc child = ((eZip . rootLabel) child ++ fst acc, snd acc ++ [scopeTree (fst acc) child])
          childScope = snd $ foldl folder (eZip effect ++ parent, []) childs
          eZip (p, l) = fmap (, l) p

linkScope :: Tree ([ScopeEffect], Scope) -> Result (Tree Scope)
linkScope = sequenceErrors JoinedError . fmap (first scopeError . result)
    where result (request, scope) = traverse (\r -> findEither r scope >>= return . (,) r) request
          scopeError (VariableScope var) = VariableNotFound var
          scopeError (LabelScope label)  = LabelNotFound label

declarationScope :: Program d -> Scope
declarationScope p@(Program declarations _) = (\(el, id) -> (dscope el, DeclarationLink (void el) id)) <$> zip declarations ids
    where ids = let (Node _ forest) = progId p in fmap rootLabel forest

linkProgram' :: Program d -> Result (Tree Scope)
linkProgram' program = linkScope (mzip requests scope)
    where (changes, requests) = munzip (localScope program)
          scope = scopeTree (declarationScope program) (mzip changes (linksTree program))

linkProgram :: Program NodeId -> Result (Program (ScopeLink, Scope))
linkProgram p@(Program _ i) =
    do st <- linkProgram' p
       let idTree = Node i (selectorProg getDeclData extract p)
       let idSt = mzip idTree (mzip (linksTree p) st)
       return $ fmap (\i -> snd . fromJust $ find ((==) i . fst) idSt) p -- dirty hack :(
