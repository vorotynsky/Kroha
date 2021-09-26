module Kroha where

import           Control.Monad.Zip     (mzip)
import           Data.Bifunctor        (first)
import           Data.Tree             (Tree (..))

import           Kroha.Parser          (parse)
import           Kroha.Ast             (FrameElement (Instructions), selectorProg)
import           Kroha.Scope           (linkProgram, linksTree)
import           Kroha.Types           (resolve, typeCastsTree, TypeConfig(..))
import           Kroha.Stack           (stack)
import           Kroha.Instructions    (instructions)
import           Kroha.Backends.Common (runBackend, Backend(typeConfig))
import           Kroha.Backends.Nasm   (nasm)
import           Data.Functor          (($>))


kroha :: String -> Either String String
kroha src = first show compile
    where compile = do
                    program <- parse src
                    scopes  <- linkProgram program
                    let programTree = Node (Instructions []) (selectorProg (const $ Instructions []) ($>) program)
                    let tc = (typeConfig nasm)
                    casts   <- (typeCastsTree tc $ mzip (linksTree program) scopes)
                    types   <- resolve tc casts
                    let stackRanges = stack tc program
                    let prepared = instructions stackRanges scopes program
                    return (runBackend nasm prepared)
