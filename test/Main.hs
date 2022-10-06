import Exercises.One.Spec
import Exercises.Two.Spec

import System.Exit
import Test.HUnit

main :: IO ()
main = do
  results <-
    runTestTT $
      test (oneTests ++ twoTests)
  if errors results + failures results == 0
    then putStrLn "Tests passed."
    else die "Tests failed."
