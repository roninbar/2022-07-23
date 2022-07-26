module Main
  ( main
  ) where

import           System.Random (randomRIO)
import           Text.Printf   (printf)

main :: IO ()
main = do
  let a = 0
  let b = 1000
  secret <- randomRIO (a + 1, b - 1)
  printf "Guess my number (it's between %d and %d): " a b
  n <- countGuesses secret (a, b) 1
  printf "That's right! It took you %d guesses.\n" n

countGuesses :: Int -> (Int, Int) -> Int -> IO Int
countGuesses secret (a, b) n = do
  guess <- read <$> getLine
  if guess == secret
    then return n
    else do
      let a' = if secret < guess then a else max guess a
      let b' = if guess < secret then b else min guess b
      printf "Too %s (it's between %d and %d). Try again (#%d): " (if guess < secret then "small" else "big") a' b' (n + 1)
      countGuesses secret (a', b') (n + 1)
