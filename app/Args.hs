module Args where

import Options.Applicative

import Kroha.Backends.Common
import Kroha.Backends.Nasm

data Options = Options 
    { backend :: Backend 
    , files   :: [FilePath] }

backends = [("nasm16", nasm 16), ("nasm32", nasm 32), ("nasm64", nasm 64)]

toRight _ (Just x) = Right x
toRight x Nothing  = Left x

backendParser = option (eitherReader (\x -> toRight "Unknown backend" $ lookup x backends))
    ( long "assembly" 
    <> metavar "BACKEND" 
    <> help "Specify backend"
    <> value (nasm 64) )

optionsParser :: Parser Options
optionsParser = Options
    <$> backendParser
    <*> some (argument str (metavar "FILES..."))

readOptions :: IO Options
readOptions = execParser opts
    where opts = info (optionsParser <**> helper) 
                  ( fullDesc
                  <> progDesc "Compiles programs written in Kroha"
                  <> header "Kroha language" )
