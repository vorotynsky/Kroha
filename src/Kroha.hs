module Kroha where

import Kroha.Parser (parse)

kroha :: String -> Either String String
kroha src = fmap show . parse $ src
