-- Copyright (c) 2020 Vorotynsky Maxim

module Main where

import           Data.Maybe
import           System.Environment
import           System.IO

import           HLasm.Ast
import           HLasm.Parser

parseAll :: String -> String
parseAll = fromMaybe "Parse error" . fmap show . parse

join :: String -> [String] -> String
join s []      = ""
join s [x]     = x
join s (x:xs)  = x ++ s ++ join s xs

main :: IO ()
main = do
    args <- getArgs
    contents <- sequence . fmap readFile $ args
    putStrLn . join "\n\n" . fmap (parseAll) $ contents

