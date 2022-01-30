module Main where

import           Data.Either        (partitionEithers)
import           Data.List          (intercalate)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           Kroha
import           Kroha.Backends.Registers 

parse :: [(String, String)] -> IO String
parse contents = 
    do regFile <- readRegFile
       let (errors, results) = partitionEithers . fmap (uncurry (kroha regFile)) $ contents
       errors <- traverse putStrLn errors
       if null errors then pure () else exitFailure
       return $ intercalate "\n\n" results

main :: IO ()
main = do
    args <- getArgs
    contents <- mapM readFile args
    parsed <- parse (zip args contents)
    putStrLn "; build with Kroha\n; see: https://github.com/vorotynsky/Kroha \n"
    putStrLn parsed          
