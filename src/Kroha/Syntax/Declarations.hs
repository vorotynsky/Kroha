-- Copyright (c) 2021 Vorotynsky Maxim

module Kroha.Syntax.Declarations where

import Data.Graph (Forest, Tree(..))
import Data.List (mapAccumR)
import Control.Comonad (($>), Comonad (extract))
import Control.Monad.Zip (mzipWith)

import Kroha.Syntax.Primitive
import Kroha.Syntax.Statements

data Declaration d
    = Frame Label (FrameElement d) d
    | GlobalVariable VariableName TypeName Literal d
    | ConstantVariable VariableName TypeName Literal d
    | ManualFrame Label InlinedCode d
    | ManualVariable VariableName TypeName InlinedCode d
    deriving (Show, Eq, Functor, Foldable, Traversable)

data Program d = Program [Declaration d] d
    deriving (Show, Eq, Functor, Foldable, Traversable)

getDeclData :: Declaration d -> d
getDeclData (Frame _ _ d)              = d
getDeclData (GlobalVariable _ _ _ d)   = d
getDeclData (ConstantVariable _ _ _ d) = d
getDeclData (ManualFrame _ _ d)        = d
getDeclData (ManualVariable _ _ _ d)   = d

selectorProg :: (Declaration d -> a) -> (FrameElement d -> a) -> Program d -> Forest a
selectorProg df sf (Program declarations _) = fmap mapper declarations
    where mapper d@(Frame _ frame _)        = Node (df d) [selector sf frame]
          mapper declaration                = Node (df declaration) []

type NodeId = Int

genId :: Program d -> Program NodeId
genId (Program decls _) = Program (snd $ mapAccumR declId 1 decls) 0
    where genId'' = mapAccumR (\ac b -> (ac + 1, ac))
          declId begin (Frame l fe _) = let (acc, fe') = genId'' (begin + 1) fe in (acc, Frame l fe' begin)
          declId begin d  = (begin + 1, d $> begin)

progId :: Program d -> Tree NodeId
progId program = Node 0 $ selectorProg getDeclData extract (genId program)

dzip :: Declaration a -> Declaration b -> Declaration (a, b)
dzip (Frame la fea _a)              (Frame lb feb _b)              |  la          ==  lb          = Frame la (tzip fea feb)   (_a, _b)
dzip (GlobalVariable va ta la _a)   (GlobalVariable vb tb lb _b)   | (va, ta, la) == (vb, tb, lb) = GlobalVariable   va ta la (_a, _b)
dzip (ConstantVariable va ta la _a) (ConstantVariable vb tb lb _b) | (va, ta, la) == (vb, tb, lb) = ConstantVariable va ta la (_a, _b)
dzip (ManualFrame la ca _a)         (ManualFrame lb cb _b)         | (la, ca)     == (lb, cb)     = ManualFrame la ca         (_a, _b)
dzip (ManualVariable va ta ca _a)   (ManualVariable vb tb cb _b)   | (va, ta, ca) == (vb, tb, cb) = ManualVariable   va ta ca (_a, _b)
dzip _ _ = error "can't zip different declarations"

pzip :: Program a -> Program b -> Program (a, b)
pzip (Program da _a) (Program db _b) = Program (mzipWith dzip da db) (_a, _b)

pzip3 :: Program a -> Program b -> Program c -> Program (a, b, c)
pzip3 a b c = fmap (\((a, b), c) -> (a, b, c)) (pzip (pzip a b) c)

