module Main where

import ADT
import Control.Monad
import Control.Monad.Except
import DataStructures
import Functions
import Monads
import PatternMatching
import TypeClasses

readInts :: String -> [Int]
-- words: Create a Array of String from String
-- read: Casts strings to another type
readInts s = let ws = words s in map read ws

r = head

--r t = fromS

minMax :: Ord a => [a] -> Maybe (a, a)
-- foldr: Reduction function, foldr(function, initValue, list)
-- \: For lambda function
minMax (head : tail) =
  Just $
    foldr
      (\number (min, max) -> (if number < min then number else min, if number > max then number else max))
      (head, head)
      tail
minMax _ = Nothing

--let x = [1, 2, 3, 4, 5] in print(foldr (\a b -> a + b) 0 x)

loadFile = do
  contents <- readFile "/Users/will/Documents/workspaces/will-workspace/hello-world-haskell/src/numbers.txt"
  let values = readInts contents
      count = length values
      total = sum values
      mean = fromIntegral total / fromIntegral count
      range = minMax values
  print values
  print count
  print total
  print mean
  print range

main :: IO ()
main = do
  e <- execute "hello4"
  print e
  md

--main = safeDivIo 4 2 `catchError` (\e -> return 1) >>= print

safeDivIo :: Int -> Int -> IO Int
safeDivIo a b = do
  return (a `div` b)

safeDiv :: Int -> Int -> Maybe Int
safeDiv a b = do
  guard $ b /= 0 -- guard requires Control.Monad, returns Pure Monad if condition is met, but Empty Monad.
  return (a `div` b)
