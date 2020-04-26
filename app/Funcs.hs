-- Copyright (c) 2020 Vorotynsky Maxim

module Funcs where

err :: e -> (a -> Maybe b) -> a -> Either e b
err e f = err e . f
    where err _ (Just x) = Right x
          err x Nothing  = Left  x

get :: Either a a -> a
get (Left x)  = x
get (Right x) = x

join :: String -> [String] -> String
join s []     = ""
join s [x]    = x
join s (x:xs) = x ++ s ++ join s xs
