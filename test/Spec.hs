import Case        ( tests )
import Test.HUnit  ( runTestTT, errors, failures )
import System.Exit ( exitFailure, exitSuccess )

cases =
    [ "frameVariables"
    , "variableDeclarations"
    , "functionDeclaration"
    , "functionCall"
    , "if"
    , "ifelse"
    , "nestedIfs"
    , "loops" ]


main :: IO ()
main = do
    results <- runTestTT (tests cases)
    if errors results + failures results == 0
        then exitSuccess
        else exitFailure
