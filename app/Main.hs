module Main where

import           Data.Either        (partitionEithers)
import           Data.List          (intercalate)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import           Args
import           Kroha
import           Kroha.Backends.Common

write :: Output -> String -> IO ()
write (StdOut   ) content = putStrLn content
write (File path) content = writeFile path content

process :: Backend -> [(String, String)] -> IO String
process backend contents = 
    do let (errors, results) = partitionEithers . fmap (uncurry $ kroha backend) $ contents
       errors <- traverse putStrLn errors
       if null errors then pure () else exitFailure
       return $ intercalate "\n\n" results

main :: IO ()
main = do
    options <- readOptions
    contents <- mapM readFile (files options)
    parsed <- process (backend options) (zip (files options) contents)
    let result = "; build with Kroha\n; see: https://github.com/vorotynsky/Kroha \n\n" ++ parsed
    write (output options) result
