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
import Kroha.Parser.Lexer

import Paths_Kroha

type RegFile = [Register]

--- Parser ---

range = Range <$> nat <*> (symbol "-" *> nat)

reg = (,) <$> (name <* symbol ":") <*> range

purpose = parser General "general" <|> parser ReturnValue "return" <|> (Argument <$> (symbol "argument" *> nat))
          <|> (symbol "stack" *> (symbol "base" $> StackBase <|> symbol "pointer" $> StackPointer))
    where parser x s = fmap (const x) $ symbol s

regDef = do
    symbol "register"
    rName <- name
    for <- (symbol "for" *> purpose) <|> pure General
    layout <- braces (many (reg <* Kroha.Parser.Lexer.end))

    return (Register rName for layout)

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
