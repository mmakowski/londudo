module Main where
import Test.Framework (Test, defaultMainWithArgs)
-- test modules
import qualified LondudoTest (allTests)

main :: IO () 
main = defaultMainWithArgs tests ["--plain"]

tests :: [Test]
tests = LondudoTest.allTests
