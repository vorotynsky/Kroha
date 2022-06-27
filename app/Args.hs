module Args where

import Options.Applicative

import Kroha.Backends.Common
import Kroha.Backends.Nasm

data Options = Options 
    { files :: [FilePath] }

optionsParser :: Parser Options
optionsParser = Options
    <$> some (argument str (metavar "FILES..."))

readOptions :: IO Options
readOptions = execParser opts
    where opts = info (optionsParser <**> helper) 
                  ( fullDesc
                  <> progDesc "Compiles programs written in Kroha"
                  <> header "Kroha language" )
