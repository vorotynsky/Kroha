-- Copyright (c) 2020 Vorotynsky Maxim

module HLasm.Scope 
( semantic
, SemanticTree(..)
, ScopeData(..)
, VariableData(..)
, LabelData(..)
, findVar
, findLabel
, Scope(..)
, elementToScope) where

import           Control.Monad.Zip
import           Data.Tree
import           HLasm.Ast

data ScopeData = FluentScope
    | IntroduceVariable HLValuable
    | IntroduceLabel Label
    deriving (Show, Eq)

type ScopeTree = Tree ScopeData

elementToScope :: HLElement -> ScopeData
elementToScope (InstructionSet)            = FluentScope
elementToScope (VariableDeclaration value) = IntroduceVariable value
elementToScope (Frame (Just label))        = IntroduceLabel label
elementToScope (Frame Nothing)             = FluentScope
elementToScope (If label)                  = IntroduceLabel label
elementToScope (IfBranch _)                = FluentScope
elementToScope (While label)               = IntroduceLabel label
elementToScope (DoWhile label)             = IntroduceLabel label
elementToScope (Break _)                   = FluentScope
elementToScope (Call _ _)                  = FluentScope
elementToScope (Assigment _ _)             = FluentScope
elementToScope (AssemblyCall _)            = FluentScope

data Scope = 
    Root
    | Scope
        { scopeData    :: ScopeData
        , scopeParent  :: Scope
        , scopeElement :: HLElement }
    deriving (Show, Eq)

newtype ScopedElement = ScopedElement (HLElement, ScopeData)

instance Semigroup (ScopedElement) where
    a <> b = undefined
instance Monoid (ScopedElement) where
    mempty = ScopedElement (InstructionSet, FluentScope)

foldz :: (c -> a) -> (a -> b -> c) -> a -> [b] -> [c]
foldz _ _ _ [ ]    = []
foldz g f a [x]    = [f a x]
foldz g f a (x:xs) = elem : (foldz g f (g elem) xs)
    where elem = f a x

fromScopeData :: Scope -> Tree (HLElement, ScopeData) -> Tree (HLElement, Scope)
fromScopeData s t@(Node (el@InstructionSet, sd) xs@(_:_)) =  -- non-empty InstructionSet
    Node (el, currScope) (foldz (\(Node (_,s) _) -> s) (fromScopeData) s xs)
        where currScope = Scope sd s el
fromScopeData s (Node (el, sd) xs) = Node (el, currScope) (fmap (fromScopeData currScope) xs)
    where currScope = Scope sd s el

newtype VariableData = VariableData (VariableName, HLElement) deriving (Show)
newtype LabelData    = LabelData    (Label,        HLElement) deriving (Show)

findVar :: Scope -> VariableName -> Maybe VariableData
findVar Root _ = Nothing
findVar (Scope {scopeData = (IntroduceVariable var), scopeElement = el}) name | valuableName var == name =
    Just $ VariableData (name, el)
findVar (Scope {scopeParent = p}) name = findVar p name

findLabel :: Scope -> VariableName -> Maybe LabelData
findLabel Root _ = Nothing
findLabel (Scope {scopeData = (IntroduceLabel lbl), scopeElement = el}) label | lbl == label =
    Just $ LabelData (lbl, el)
findLabel (Scope {scopeParent = p}) label = findLabel p label

type SemanticTree = Tree (HLElement, [VariableData], [LabelData])

semantic :: SyntaxTree -> Maybe SemanticTree
semantic t = traverse process . fromScopeData Root $ mzip t (fmap elementToScope t)
    where used f u s = sequence . fmap (f s) . u
          process (el, scope) = do
              vars <- used findVar usedVariables scope el
              labels <- used findLabel usedLabels scope el
              Just (el, vars, labels)
