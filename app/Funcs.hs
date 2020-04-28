-- Copyright (c) 2020 Vorotynsky Maxim

module Funcs where

import           Control.Monad.Zip

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

rezip :: MonadZip m => m (a, b) -> m (a, c) -> m (a, b, c)
rezip b c = mzipWith (\(a, b) (_, c) -> (a, b, c)) b c

ziplify :: MonadZip m => (m a -> m (a, c)) -> m (a, b) -> m (a, b, c)
ziplify f b = rezip b (f pa)
    where (pa, pb) = munzip b
