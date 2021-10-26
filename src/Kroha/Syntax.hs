-- Copyright (c) 2020 - 2021 Vorotynsky Maxim

module Kroha.Syntax where

import Control.Comonad
import Data.Tree
import Data.List (mapAccumR)
import Control.Monad.Zip (mzipWith)

type VariableName = String
type RegisterName = String
type InlinedCode  = String
type Label = String

data TypeName 
    = TypeName String
    | PointerType TypeName    
    deriving (Show, Eq)

data Literal
    = IntegerLiteral Int
    deriving (Show, Eq)

data LValue
    = VariableLVal VariableName
    | RegisterLVal RegisterName
    deriving (Show, Eq)

data RValue
    = AsRValue LValue
    | RLiteral Literal
    deriving (Show, Eq)

data LocalVariable
    = StackVariable VariableName TypeName
    | RegisterVariable VariableName RegisterName
    deriving (Show, Eq)

data Comparator
    = Equals    | NotEquals
    | Greater   | Less
    deriving (Show, Eq)

newtype Condition = Condition (RValue, Comparator, RValue)
    deriving (Show, Eq)

data FrameElement d
    = Instructions [FrameElement d] d
    | VariableDeclaration LocalVariable d
    | If Label Condition (FrameElement d) (FrameElement d) d
    | Loop Label (FrameElement d) d
    | Break Label d
    | Call Label [RValue] d
    | Assignment LValue RValue d
    | Inline InlinedCode d
    deriving (Show, Eq, Functor, Foldable, Traversable)


data Declaration d
    = Frame Label (FrameElement d) d
    | GlobalVariable VariableName TypeName Literal d
    | ConstantVariable VariableName TypeName Literal d
    | ManualFrame Label InlinedCode d
    | ManualVariable VariableName TypeName InlinedCode d
    deriving (Show, Eq, Functor, Foldable, Traversable)

data Program d = Program [Declaration d] d
    deriving (Show, Eq, Functor, Foldable, Traversable)

childs :: FrameElement d -> [FrameElement d]
childs (Instructions xs _)       = xs
childs (VariableDeclaration x _) = []
childs (If _ _ b e _)            = [b, e]
childs (Loop _ b _)              = [b]
childs (Break _ _)               = []
childs (Call _ _ _)              = []
childs (Assignment _ _ _)        = []
childs (Inline _ _)              = []

getDeclData :: Declaration d -> d
getDeclData (Frame _ _ d)              = d
getDeclData (GlobalVariable _ _ _ d)   = d
getDeclData (ConstantVariable _ _ _ d) = d
getDeclData (ManualFrame _ _ d)        = d
getDeclData (ManualVariable _ _ _ d)   = d



selector :: (FrameElement d -> a) -> FrameElement d -> Tree a
selector s = unfoldTree (\e -> (s e, childs e))

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

instance Comonad FrameElement where
    duplicate node@(Instructions c _)        = Instructions (map duplicate c) node
    duplicate node@(VariableDeclaration v _) = VariableDeclaration v node
    duplicate node@(If l c i e _)            = If l c (duplicate i) (duplicate e) node
    duplicate node@(Loop l b _)              = Loop l (duplicate b) node
    duplicate node@(Break l _)               = Break l node
    duplicate node@(Call l a _)              = Call l a node
    duplicate node@(Assignment l r _)        = Assignment l r node
    duplicate node@(Inline c _)              = Inline c node

    extract (Instructions _ d)        = d
    extract (VariableDeclaration _ d) = d
    extract (If _ _ _ _ d)            = d
    extract (Loop _ _ d)              = d
    extract (Break _ d)               = d
    extract (Call _ _ d)              = d
    extract (Assignment _ _ d)        = d
    extract (Inline _ d)              = d

tzip :: FrameElement a -> FrameElement b -> FrameElement (a, b)
tzip (Instructions ca _a)           (Instructions cb _b)                               = Instructions (uncurry tzip <$> zip ca cb) (_a, _b)
tzip (VariableDeclaration va _a)    (VariableDeclaration vb _b) |  va      ==  vb      = VariableDeclaration va                    (_a, _b)
tzip (If la ca ia ea _a)            (If lb cb ib eb _b)         | (la, ca) == (lb, cb) = If la ca  (tzip ia ib) (tzip ea eb)       (_a, _b)
tzip (Loop la ba _a)                (Loop lb bb _b)             |  la      ==  lb      = Loop       la (tzip ba bb)                (_a, _b)
tzip (Break la _a)                  (Break lb _b)               |  la      ==  lb      = Break      la                             (_a, _b)
tzip (Call la aa _a)                (Call lb ab _b)             | (la, aa) == (lb, ab) = Call       la aa                          (_a, _b)
tzip (Assignment la ra _a)          (Assignment lb rb _b)       | (la, ra) == (lb, rb) = Assignment la ra                          (_a, _b)
tzip (Inline ca _a)                 (Inline cb _b)              |  ca      ==  cb      = Inline     ca                             (_a, _b)
tzip _ _ = error "can't zip different frame elements"

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
