module Main where

import           Data.Either.Extra  (fromEither)
import           Data.List          (intercalate)
import           Kroha
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    contents <- sequence . fmap readFile $ args
    _ <- putStrLn "; build with Kroha\n; see: https://github.com/vorotynsky/Kroha \n"
    putStrLn . intercalate "\n\n" . fmap (fromEither . kroha) $ contents
