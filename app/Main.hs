-- Copyright (c) 2020 Vorotynsky Maxim

module Main where

import           System.Environment (getArgs)
import           Control.Monad.Zip
import           Data.Bifunctor (first)

import           Funcs
import           HLasm.Parser
import           HLasm.Scope (semantic)
import           HLasm.Types (typeCheck)
import           HLasm.Frame (StackFrame(Root), buildStackFrames)
import           HLasm.Instructions (instructions, BackEnd(..), runBackend)
import           HLasm.Backend.Nasm

parseAll :: String -> String
parseAll = get . first show . pipeline
    where pipeline src = 
            do parsed   <- parse src
               semantic <- semantic parsed
               _        <- typeCheck semantic
               stack    <- Right $ (buildStackFrames Root) parsed
               tree     <- Right $ mzipWith (\(e, v, l) (_, sf) -> (e, v, l, sf)) semantic stack 
               instructions <- instructions tree
               runBackend nasm instructions

main :: IO ()
main = do
    args <- getArgs
    contents <- sequence . fmap readFile $ args
    putStrLn . join "\n\n" . fmap (parseAll) $ contents
