module Kroha where

import           Control.Monad.Zip (munzip, mzip)
import           Data.Bifunctor    (first)
import           Data.Tree         (Tree (..), drawTree)

import           Kroha.Ast         (FrameElement (..), selectorProg)
import           Kroha.Parser      (parse)
import           Kroha.Scope       (linkProgram, linksTree)
import           Kroha.Stack       (stack)
import           Kroha.Types       (resolve, typeCasts)
import           Kroha.Instructions(instructions)
import           Kroha.Backends.Nasm (runNasm)


kroha :: String -> Either String String
kroha src = compile
    where compile = do
                    program <- first id   $ parse src
                    scopes  <- first show $ linkProgram program
                    let programTree = Node (Instructions []) (selectorProg (const $ Instructions []) id program)
                    types   <- first show $ resolve 16 . typeCasts $ mzip (linksTree program) scopes
                    let stackRanges = stack 16 program
                    let prepared = instructions stackRanges scopes program
                    return (runNasm prepared)
