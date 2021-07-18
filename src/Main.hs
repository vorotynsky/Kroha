module Main where

import           Data.Either.Extra  (fromEither)
import           Data.Either        (partitionEithers)
import           Data.List          (intercalate)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           Kroha

parse :: [String] -> IO String
parse contents = do
                 let (errors, results) = partitionEithers . fmap kroha $ contents
                 errors <- traverse putStrLn errors
                 if null errors then pure () else exitFailure
                 return $ intercalate "\n\n" results

main :: IO ()
main = do
    args <- getArgs
    contents <- mapM readFile args
    parsed <- parse contents
    putStrLn "; build with Kroha\n; see: https://github.com/vorotynsky/Kroha \n"
    putStrLn parsed          
