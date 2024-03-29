-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

module Kroha.Scope where

import           Control.Comonad     (extract)
import           Control.Monad.Zip   (munzip, mzip)
import           Data.Foldable       (find)
import           Data.Maybe          (fromJust, mapMaybe)
import           Data.Tree           (Tree (..))

import           Kroha.Errors
import           Kroha.Syntax.Syntax


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

scope :: FrameElement d -> ([PushToScope], [RequestFromScope])
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
toRight x Nothing  = Left x

findEither k = toRight k . lookup k

data ScopeLink
    = ElementLink (FrameElement NodeId)
    | DeclarationLink (Declaration NodeId)
    | RootProgramLink (Program NodeId)
    deriving (Show)

localScope :: Program d -> Tree ([PushToScope], [RequestFromScope])
localScope program = Node ([], []) (selectorProg dscope' scope program)

linksTree :: Program NodeId -> Tree ScopeLink
linksTree program = Node (RootProgramLink program) $ selectorProg DeclarationLink ElementLink program

scopeTree :: Scope -> Tree ([PushToScope], ScopeLink) -> Tree Scope
scopeTree parent (Node effect children) = Node (eZip effect ++ parent) childScope
    where folder acc child = ((eZip . rootLabel) child ++ fst acc, snd acc ++ [scopeTree (fst acc) child])
          childScope = snd $ foldl folder (eZip effect ++ parent, []) children
          eZip (p, l) = fmap (, l) p

linkScope :: Tree (NodeId, ([ScopeEffect], Scope)) -> Result (Tree Scope)
linkScope = sequenceE id . fmap (\(i, d) -> sequenceE (fmap (scopeError i)) $ result d)
    where result (request, scope) = fmap (\r -> (,) r <$> findEither r scope) request
          sequenceE f = sequenceErrors (JoinedError . f)
          scopeError i (VariableScope var) = VariableNotFound var i
          scopeError i (LabelScope label)  = LabelNotFound label  i

declarationScope :: Program NodeId -> Scope
declarationScope p@(Program declarations _) = fmap (\el -> (dscope el, DeclarationLink el)) declarations


linkProgram' :: Program NodeId -> Result (Tree Scope)
linkProgram' program@(Program _ i) = linkScope $ mzip ids (mzip requests scope)
    where (changes, requests) = munzip (localScope program)
          ids = Node i $ selectorProg getDeclData extract program
          scope = scopeTree (declarationScope program) (mzip changes (linksTree program))

linkProgram :: Program NodeId -> Result (Program (ScopeLink, Scope))
linkProgram p@(Program _ i) =
    do st <- linkProgram' p
       let idTree = Node i (selectorProg getDeclData extract p)
       let idSt = mzip idTree (mzip (linksTree p) st)
       return $ fmap (\i -> snd . fromJust $ find ((==) i . fst) idSt) p -- dirty hack :(
