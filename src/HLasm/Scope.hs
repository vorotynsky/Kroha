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
import           Data.Bifunctor
import           HLasm.Ast
import           HLasm.Error

data ScopeData = FluentScope
    | IntroduceVariable VariableName
    | IntroduceLabel Label
    deriving (Show, Eq)

type ScopeTree = Tree ScopeData

elementToScope :: HLElement -> ScopeData
elementToScope (Program)                      = FluentScope
elementToScope (InstructionSet)               = FluentScope
elementToScope (VariableDeclaration var _)    = IntroduceVariable var
elementToScope (RegisterDeclaration var _)    = IntroduceVariable var
elementToScope (GlobalVarDeclaration var _ _) = IntroduceVariable var
elementToScope (ConstVarDeclaration var _ _)  = IntroduceVariable var
elementToScope (FakeVariable name)            = IntroduceVariable name
elementToScope (FakeFrame name)               = IntroduceLabel name
elementToScope (Frame (Just label))           = IntroduceLabel label
elementToScope (Frame Nothing)                = FluentScope
elementToScope (If label)                     = IntroduceLabel label
elementToScope (IfBranch _)                   = FluentScope
elementToScope (While label)                  = IntroduceLabel label
elementToScope (DoWhile label)                = IntroduceLabel label
elementToScope (Break _)                      = FluentScope
elementToScope (Call _ _)                     = FluentScope
elementToScope (Assignment _ _)               = FluentScope
elementToScope (AssemblyCall _)               = FluentScope

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

chainScope s (Node (el, sd) xs) = 
    Node (el, currScope) (foldz (\(Node (_,s) _) -> s) (fromScopeData) s xs)
        where currScope = Scope sd s el

fromScopeData :: Scope -> Tree (HLElement, ScopeData) -> Tree (HLElement, Scope)
fromScopeData s t@(Node (el@Program       , sd) xs@(_:_)) = chainScope s t
fromScopeData s t@(Node (el@InstructionSet, sd) xs@(_:_)) = chainScope s t
fromScopeData s (Node (el, sd) xs) = Node (el, currScope) (fmap (fromScopeData currScope) xs)
    where currScope = Scope sd s el

newtype VariableData = VariableData (VariableName, HLElement) deriving (Show)
newtype LabelData    = LabelData    (Label,        HLElement) deriving (Show)

findVar :: Scope -> VariableName -> Either VariableName VariableData
findVar Root name = Left name
findVar (Scope {scopeData = (IntroduceVariable var), scopeElement = el}) name | var == name =
    Right $ VariableData (var, el)
findVar (Scope {scopeParent = p}) name = findVar p name

findLabel :: Scope -> Label -> Either Label LabelData
findLabel Root label = Left label
findLabel (Scope {scopeData = (IntroduceLabel lbl), scopeElement = el}) label | lbl == label =
    Right $ LabelData (lbl, el)
findLabel (Scope {scopeParent = p}) label = findLabel p label

type SemanticTree = Tree (HLElement, [VariableData], [LabelData])

semantic :: SyntaxTree -> Result SemanticTree
semantic t = traverse process . fromScopeData Root $ mzip t (fmap elementToScope t)
    where used f u s = sequence . fmap (f s) . u
          process (el, scope) = do
              vars   <- first VariableNotFound $ used findVar usedVariables scope el
              labels <- first LabelNotFound    $ used findLabel usedLabels scope el
              Right (el, vars, labels)
