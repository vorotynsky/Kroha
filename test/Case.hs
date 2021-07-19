module Case where

import Data.Either.Extra (fromEither)
import Data.Algorithm.Diff (getGroupedDiff, Diff, PolyDiff(..))
import Test.HUnit
import Kroha (kroha)

type TestName = String

toFile :: String -> TestName -> String
toFile sufix name = "test/examples/" ++ name ++ sufix

printDiff :: [Diff [String]] -> String
printDiff = concatMap show1
    where doUnlines p a = unlines (fmap (p ++) a)
          show1 (First  a)          = doUnlines "+ " a
          show1 (Second b)          = doUnlines "- " b
          show1 (Both a b) | a /= b = doUnlines "@ " a ++ doUnlines "# " b
                           | a == b = doUnlines "  " a
          show1 _                   = ""

generateError :: TestName -> String -> String -> String
generateError name expected actual = "test `" ++ name ++ "` failed" ++ "\n\ndiff:\n" ++ printDiff diff
    where diff = getGroupedDiff (lines expected) (lines actual)

assertProgram :: TestName -> String -> String -> IO ()
assertProgram name expected actual =
    if expected == actual
    then return ()
    else do
        writeFile (toFile ".actual" name) actual
        ioError (userError ("\n" ++ generateError name expected actual))


testCase :: TestName -> Test
testCase name = TestCase $ do
    [program, expected] <- mapM readFile [toFile ".kr" name, toFile ".s" name]
    let actual = fromEither $ kroha program
    assertProgram name expected actual

tests :: [TestName] -> Test
tests names = TestList $ fmap (\name -> TestLabel name (testCase name)) names
