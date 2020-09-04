module Kroha.Scope where

import Control.Monad (join)
import Control.Monad.Zip (mzip, munzip, mzipWith)
import Data.Maybe (mapMaybe)
import Data.Tree (Tree(..))

import Kroha.Ast

data ScopeEffect 
    = FluentScope
    | VariableScope VariableName
    | LabelScope Label
    deriving (Eq, Show)

requestVars :: [RValue] -> [ScopeEffect]
requestVars = mapMaybe (fmap VariableScope . rvalueScope)
    where rvalueScope (AsRValue (VariableLVal name)) = Just name
          rvalueScope _                              = Nothing 

scope :: Selector (ScopeEffect, [ScopeEffect])
scope (Instructions _)                                = (FluentScope         , [])
scope (VariableDeclaration (StackVariable name _))    = (VariableScope name  , [])
scope (VariableDeclaration (RegisterVariable name _)) = (VariableScope name  , [])
scope (If label (Condition (a, _, b)) _ _)            = (LabelScope    label , requestVars [a, b])
scope (Loop label _)                                  = (LabelScope    label , [])
scope (Break label)                                   = (FluentScope         , LabelScope label:[])
scope (Call label args)                               = (FluentScope         , LabelScope label : requestVars args)
scope (Assignment lval rval)                          = (FluentScope         , requestVars [AsRValue lval, rval])
scope (Inline _)                                      = (FluentScope         , [])

dscope :: Declaration -> ScopeEffect
dscope (Frame label _)             = LabelScope    label
dscope (GlobalVariable name _ _)   = VariableScope name
dscope (ConstantVariable name _ _) = VariableScope name
dscope (ManualFrame label _)       = LabelScope    label
dscope (ManualVariable name _ _)   = VariableScope name

dscope' d = (dscope d, [] :: [ScopeEffect])

type Scope = [(ScopeEffect, ScopeLink)]

toRight _ (Just x) = Right x
toRight x (Nothing) = Left x

findEither k = toRight k . lookup k

data ScopeLink
    = ElementLink FrameElement NodeId
    | DeclarationLink Declaration NodeId
    | RootProgramLink NodeId
    deriving (Show)

localScope :: Program -> Tree (ScopeEffect, [ScopeEffect])
localScope program = Node (FluentScope, []) (selectorProg dscope' scope program)

linksTree :: Program -> Tree ScopeLink
linksTree program = mzipWith id (Node (RootProgramLink) (selectorProg DeclarationLink ElementLink program)) (progId program)

scopeTree :: Scope -> Tree (ScopeEffect, ScopeLink) -> Tree Scope
scopeTree parent (Node effect childs) = Node (effect:parent) childScope
    where folder acc child = (rootLabel child : fst acc, snd acc ++ [scopeTree (fst acc) child])
          childScope = snd $ foldl folder (effect:parent, []) childs

linkScope :: Tree ([ScopeEffect], Scope) -> Either (ScopeEffect) (Tree Scope)
linkScope (Node (request, scope) childs) = join . fmap buildTree $ results
    where results = traverse (\r -> findEither r scope >>= return . (,) r) request
          buildTree request = sequence . traverse (Node request) $ traverse linkScope childs

declarationScope :: Program -> Scope
declarationScope p@(Program declarations) = fmap (\(el, id) -> (dscope el, DeclarationLink el id)) $ zip declarations ids
    where ids = let (Node _ forest) = progId p in fmap rootLabel forest

linkProgram :: Program -> Either (ScopeEffect) (Tree Scope)
linkProgram program = linkScope (mzip requests scope)
    where (changes, requests) = munzip (localScope program)
          scope = scopeTree (declarationScope program) (mzip changes (linksTree program))
