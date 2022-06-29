module Main where

import           Data.Either        (partitionEithers)
import           Data.List          (intercalate)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import           Args
import           Kroha
import           Kroha.Backends.Common

parse :: Backend -> [(String, String)] -> IO String
parse backend contents = 
    do let (errors, results) = partitionEithers . fmap (uncurry $ kroha backend) $ contents
       errors <- traverse putStrLn errors
       if null errors then pure () else exitFailure
       return $ intercalate "\n\n" results

main :: IO ()
main = do
    options <- readOptions
    contents <- mapM readFile (files options)
    parsed <- parse (backend options) (zip (files options) contents)
    putStrLn "; build with Kroha\n; see: https://github.com/vorotynsky/Kroha \n"
    putStrLn parsed
