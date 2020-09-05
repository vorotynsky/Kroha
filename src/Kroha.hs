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


kroha :: String -> Either String String
kroha src = compile
    where compile = do
                    program <- first id   $ parse src
                    scopes  <- first show $ linkProgram program
                    let programTree = Node (Instructions []) (selectorProg (const $ Instructions []) id program)
                    let tc = (typeConfig nasm)
                    types   <- first show $ resolve tc (typeCastsTree tc $ mzip (linksTree program) scopes)
                    let stackRanges = stack tc program
                    let prepared = instructions stackRanges scopes program
                    return (runBackend nasm prepared)
