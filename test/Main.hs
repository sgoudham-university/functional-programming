import Exercises.One.Spec (oneTests)
import Exercises.Two.Spec (twoTests)
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  printOut oneTests
  printOut twoTests

printOut :: Test -> IO ()
printOut input = do
  results <- runTestTT input
  if errors results + failures results == 0 then putStrLn "Tests Passed." else die "Tests Failed."
