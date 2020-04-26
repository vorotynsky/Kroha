-- Copyright (c) 2020 Vorotynsky Maxim

module Main where

import           Data.Tree (drawTree)
import           System.Environment (getArgs)
import           Control.Monad ((>=>))

import           Funcs
import           HLasm.Parser
import           HLasm.Scope

parseAll :: String -> String
parseAll = get . fmap (drawTree . fmap show) . pipeline
    where pipeline = err "Parse error" parse >=> err "Scope error" (semantic)

main :: IO ()
main = do
    args <- getArgs
    contents <- sequence . fmap readFile $ args
    putStrLn . join "\n\n" . fmap (parseAll) $ contents
