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

compile, test :: [String] -> IO ()

compile args = do
    contents <- mapM readFile args
    parsed <- parse contents
    putStrLn "; build with Kroha\n; see: https://github.com/vorotynsky/Kroha \n"
    putStrLn parsed          

test args = do
    let [name] = filter (/= "--test") args
    [program, expected] <- mapM readFile [name ++ ".kr", name ++ ".s"]
    actual <- parse [program]
    
    if actual == expected then do
        putStrLn "test passed"
        exitSuccess
    else do
        putStr ("test failed\n\nexpected:\n" ++ expected ++ "\n\nactual:\n" ++ actual) 
        writeFile (name ++ ".actual") actual
        exitFailure 

main :: IO ()
main = do
    args <- getArgs
    if "--test" `elem` args then test args else compile args
