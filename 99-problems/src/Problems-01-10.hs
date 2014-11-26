import Control.Monad
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test

main :: IO ()
main = do
  result <- quickCheckResult $ conjoin tests
  unless (isSuccess result) exitFailure

tests = join
  [ tests_01
  , tests_02
  -- add tests of further problems here
  ]

-- Problem 1: Find the last element of a list.
tests_01 :: [Property]
tests_01 =
  [ counterexample "problem 01 test case 01" $
      myLast [1, 2, 3, 4] == 4
  , counterexample "problem 01 test case 02" $
      myLast "xyz" == 'z'
  ]

myLast :: [a] -> a
myLast = undefined

-- Problem 2: Find the last but one element of a list.
tests_02 :: [Property]
tests_02 =
  [
  ]

myButLast :: [a] -> a
myButLast = undefined
