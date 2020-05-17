-- Copyright (c) 2020 Vorotynsky Maxim

module Main where

import           Data.Tree (drawTree)
import           System.Environment (getArgs)
import           Control.Monad ((>=>))
import           Control.Monad.Zip

import           Funcs
import           HLasm.Parser
import           HLasm.Scope (semantic)
import           HLasm.Types (typeCheck)
import           HLasm.Frame (StackFrame(Root), buildStackFrames)

parseAll :: String -> String
parseAll = get . fmap (drawTree . fmap show) . pipeline
    where pipeline src = 
            do parsed    <- err "Parse error" parse src
               semantic  <- err "Scope error" semantic parsed
               _         <- err "Type error"  typeCheck semantic
               stack     <- Right $ (buildStackFrames Root) parsed
               Right $ mzipWith (\(e, v, l) (_, sf) -> (e, v, l, sf)) semantic stack

main :: IO ()
main = do
    args <- getArgs
    contents <- sequence . fmap readFile $ args
    putStrLn . join "\n\n" . fmap (parseAll) $ contents
