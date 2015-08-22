import System.Exit
import Test.HUnit


testSuite = TestList [
    ]

main = do
    counts <- runTestTT $ TestList [testSuite]
    if errors counts > 0
    then exitWith $ ExitFailure 2
    else if failures counts > 0
         then exitWith $ ExitFailure 1
         else exitSuccess
