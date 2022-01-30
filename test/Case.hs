module Case where

import Data.Either.Extra (fromEither)
import Data.Algorithm.Diff (getGroupedDiff, Diff, PolyDiff(..))
import Test.HUnit
import Kroha (kroha)
import Data.List (dropWhileEnd, find)
import Data.Char (isSpace)
import Data.Maybe (isJust, fromJust)
import Control.Monad (join)
import Data.List.Extra (trim)
import Data.Tuple.Extra (both)
import Kroha.Backends.Registers ( readRegFile )

type TestName = String

toFile :: String -> TestName -> String
toFile sufix name = "test/" ++ name ++ sufix

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


splitLines :: String -> String -> [String]
splitLines sep text = doUntil (split1 sep) (lines text)
    where trim = dropWhileEnd isSpace . dropWhile isSpace
          split1 p t = (unlines $ takeWhile (\x -> trim x /= p) t, safeTail $ dropWhile (/= p) t)
          safeTail x = case x of [] -> []; (_:t) -> t
          doUntil f [] = []
          doUntil f xs = let (curr, tail) = f xs in curr : doUntil f tail

splitBy :: String -> String -> Maybe (String, String)
splitBy label test = case splitLines ("======= " ++ label ++ " =======") test of [a, b] -> Just(a, b); _ -> Nothing

testCase :: TestName -> Test
testCase name = TestCase $ do
    regfile <- readRegFile
    text <- readFile $ toFile ".test.kr" name
    let (program, expected) = both trim $ extract text
    let actual = trim . fromEither $ kroha regfile name program
    assertProgram name expected actual
    where extract text = fromJust . join . find isJust . fmap (`splitBy` text) $ ["nasm", "errors"]

tests :: [TestName] -> Test
tests names = TestList $ fmap (\name -> TestLabel name (testCase name)) names
