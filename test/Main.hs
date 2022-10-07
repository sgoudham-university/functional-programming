import Exercises.One.Spec (oneTests)
import Exercises.Two.Spec (twoTests)
import Test.HUnit

-- mapM_ discards the returned list, as opposed to mapM which returns the list

main :: IO ()
main = mapM_ runTestTT [oneTests, twoTests]
