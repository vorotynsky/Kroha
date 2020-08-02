module Kroha where

import Data.Bifunctor (first)
import Data.Tree (Tree(..), drawTree)
import Control.Monad.Zip (mzip)

import Kroha.Parser (parse)
import Kroha.Ast (selectorProg)
import Kroha.Scope (linkProgram, ScopeLink(..))

kroha :: String -> Either String String
kroha src = fmap (drawTree . fmap show) compile
    where compile = do
                    program <- parse src
                    scopes <- first show $ linkProgram program
                    let ptree = Node RootProgramLink (selectorProg DeclarationLink ElementLink program)
                    return (mzip ptree scopes)
