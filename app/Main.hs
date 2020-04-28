-- Copyright (c) 2020 Vorotynsky Maxim

module Main where

import           Data.Tree (drawTree)
import           System.Environment (getArgs)
import           Control.Monad ((>=>))

import           Funcs
import           HLasm.Parser
import           HLasm.Scope
import           HLasm.Types
import           HLasm.Frame

parseAll :: String -> String
parseAll = get . fmap (drawTree . fmap show) . pipeline
    where pipeline = err "Parse error" parse 
            >=> err "Scope error" (semantic) 
            >=> err "Type error" typeCheck
            >=> Right . ziplify buildFrameTree . fmap (\(a, b, c) -> (a, (b, c)))

main :: IO ()
main = do
    args <- getArgs
    contents <- sequence . fmap readFile $ args
    putStrLn . join "\n\n" . fmap (parseAll) $ contents
