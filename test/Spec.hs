import Case        ( tests )
import Test.HUnit  ( runTestTT, errors, failures )
import System.Exit ( exitFailure, exitSuccess )

cases =
    [ "examples/frameVariables"
    , "examples/variableDeclarations"
    , "examples/functionDeclaration"
    , "examples/functionCall"
    , "examples/if"
    , "examples/ifelse"
    , "examples/nestedIfs"
    , "examples/loops"
    
    , "errors/scopeErrors"
    , "errors/typeErrors" ]


main :: IO ()
main = do
    results <- runTestTT (tests cases)
    if errors results + failures results == 0
        then exitSuccess
        else exitFailure
