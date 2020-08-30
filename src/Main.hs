module Main where

import           Kroha
import           System.Environment (getArgs)

get :: Either a a -> a
get (Left  a) = a
get (Right a) = a

join :: String -> [String] -> String
join s []     = ""
join s [x]    = x
join s (x:xs) = x ++ s ++ join s xs

main :: IO ()
main = do
    args <- getArgs
    contents <- sequence . fmap readFile $ args
    _ <- putStrLn "-- build with Kroha\n-- see: https://github.com/vorotynsky/Kroha \n"
    putStrLn . join "\n\n" . fmap (get . kroha) $ contents
