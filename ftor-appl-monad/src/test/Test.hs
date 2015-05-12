{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Char (isDigit)
import Data.Functor
import Control.Applicative
import Control.Monad
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test

import ParserCombinators

main :: IO ()
main = do
  result <- quickCheckResult $ conjoin tests
  unless (isSuccess result) exitFailure

tests :: [Property]
tests =
  [ counterexample "parse integer" $
      \(num :: NonNegative Int) ->
        let
          NonNegative int = num
          input = show int
        in result parseNum input == [int]
  , let list = ["one", "two", "three"]
    in counterexample ("parse length-prefixed list " ++ show list) $
      let input = join $ map encodeLP list
      in result (many parseLP) input == [list]
{-
  , counterexample "parse list of length-prefixed strings" $
      \(list :: [String]) ->
        let input = join $ map encodeLP list
        in result (many parseLP) input == [list]
-}
  ]

encodeLP :: String -> String
encodeLP s = show (length s) ++ ":" ++ s

parseLP :: Parser String
parseLP = do
  len <- parseNum
  _ <- char ':'
  str <- parseFixLen len
  return str

parseNum :: Parser Int
parseNum = fmap read $ many digit

parseFixLen :: Int -> Parser String
parseFixLen len = satisfy ((len==) . length) $ anyString

anyString :: Parser String
anyString = many anyChar

char :: Char -> Parser Char
char c = satisfy (c==) anyChar

digit :: Parser Char
digit = satisfy isDigit anyChar

anyChar :: Parser Char
anyChar = Parser $ \case
  (x:xs) -> [(x, xs)]
  [] -> empty
