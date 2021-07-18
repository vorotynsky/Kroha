module Case where

import Data.Either.Extra (fromEither)
import Test.HUnit
import Kroha (kroha)

toFile :: String -> String -> String
toFile sufix name = "test/examples/" ++ name ++ sufix

assertProgram :: String -> String -> String -> IO ()
assertProgram name expected actual = do
    if expected == actual 
    then return () 
    else do
        writeFile (toFile ".actual" name) actual
        ioError (userError ("HUnit: " ++ name ++ " failed"))


testCase :: String -> Test
testCase name = TestCase $ do
    [program, expected] <- mapM readFile [toFile ".kr" name, toFile ".s" name]
    let actual = fromEither $ kroha program
    assertProgram name expected actual

tests :: [String] -> Test
tests names = TestList $ fmap (\name -> TestLabel name (testCase name)) names
