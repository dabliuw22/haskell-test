module Main where

import ADT
import DataStructures
import Functions
import PatternMatching
import TypeClasses

readInts :: String -> [Int]
-- words: Create a Array of String from String
-- read: Casts strings to another type
readInts s = let ws = words s in map read ws

minMax :: Ord a => [a] -> Maybe(a, a)
-- foldr: Reduction function, foldr(function, initValue, list)
-- \: For lambda function
minMax (head : tail) = Just $ foldr
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
main = loadFile

f = do
  i <- [1..]
  return (i * 2)