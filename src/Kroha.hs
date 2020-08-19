module Kroha where

import           Control.Monad.Zip (munzip, mzip)
import           Data.Bifunctor    (first)
import           Data.Tree         (Tree (..), drawTree)

import           Kroha.Ast         (FrameElement (..), selectorProg)
import           Kroha.Parser      (parse)
import           Kroha.Scope       (linkProgram, linksTree)
import           Kroha.Stack       (stackFrames)
import           Kroha.Types       (resolve, typeCasts)


kroha :: String -> Either String String
kroha src = do (sizes, frames) <- fmap munzip compile
               let tree = drawTree $ Node "program" ((fmap . fmap) show frames)
               return (show sizes ++ "\n" ++ tree)
    where compile = do
                    program <- first id   $ parse src
                    scopes  <- first show $ linkProgram program
                    let programTree = Node (Instructions []) (selectorProg (const $ Instructions []) id program)
                    types   <- first show $ resolve 16 . typeCasts $ mzip (linksTree program) scopes
                    let stack = stackFrames 16 program
                    return stack
