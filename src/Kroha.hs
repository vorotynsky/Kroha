module Kroha where

import Data.Bifunctor (first)
import Data.Tree (Tree(..), drawTree)
import Control.Monad.Zip (mzip)

import Kroha.Parser (parse)
import Kroha.Ast (selectorProg, FrameElement(..))
import Kroha.Scope (linkProgram, linksTree)
import Kroha.Types (resolve, typeCasts)

kroha :: String -> Either String String
kroha src = fmap (drawTree . fmap show) compile
    where compile = do
                    program <- first id   $ parse src
                    scopes  <- first show $ linkProgram program
                    let programTree = Node (Instructions []) (selectorProg (const $ Instructions []) id program)
                    types   <- first show $ resolve 16 . typeCasts $ mzip (linksTree program) scopes
                    return (types)
