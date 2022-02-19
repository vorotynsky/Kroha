-- Copyright (c) 2021 Vorotynsky Maxim

module Kroha.Syntax.Statements where

import           Control.Comonad
import           Data.Tree              (Tree, unfoldTree)

import           Kroha.Syntax.Primitive

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

children :: FrameElement d -> [FrameElement d]
children (Instructions xs _)       = xs
children (VariableDeclaration x _) = []
children (If _ _ b e _)            = [b, e]
children (Loop _ b _)              = [b]
children (Break _ _)               = []
children (Call _ _ _)              = []
children (Assignment _ _ _)        = []
children (Inline _ _)              = []

selector :: (FrameElement d -> a) -> FrameElement d -> Tree a
selector s = unfoldTree (\e -> (s e, children e))

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
