module Kroha.Backends.Registers
( Range(..)
, RegPurpose(..)
, Register(..)
, RegFile
, parseRegFile
, readRegFile
) where

import Text.Megaparsec
import Data.Functor (($>))
import Data.Bifunctor (first)
import System.Exit (die)

import Kroha.Syntax.Syntax (Register(..), RegPurpose(..), Range(..))
import Kroha.Parser.Declarations (regDef)

import Paths_Kroha

type RegFile = [Register]

--- Parser ---

regFile = many regDef <* eof

parseRegFile :: String -> Either String RegFile
parseRegFile = first errorBundlePretty . runParser regFile "regfile"

sure :: Either String a -> IO a
sure (Left msg) = die msg
sure (Right x)  = return x

readRegFile :: IO RegFile
readRegFile = 
    do filename <- getDataFileName "lib/regfile"
       content <- readFile filename
       sure $ parseRegFile content
