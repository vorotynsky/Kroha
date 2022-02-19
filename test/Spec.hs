import           Case        (tests)
import           System.Exit (exitFailure, exitSuccess)
import           Test.HUnit  (errors, failures, runTestTT)

cases =
    [ "examples/frameVariables"
    , "examples/variableDeclarations"
    , "examples/functionDeclaration"
    , "examples/functionCall"
    , "examples/if"
    , "examples/ifelse"
    , "examples/nestedIfs"
    , "examples/loops"
    , "examples/semicolons"
    , "examples/elseif"

    , "errors/scopeErrors"
    , "errors/typeErrors" ]


main :: IO ()
main = do
    results <- runTestTT (tests cases)
    if errors results + failures results == 0
        then exitSuccess
        else exitFailure
