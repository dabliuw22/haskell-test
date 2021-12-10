module DataStructures (mod2, dt) where

mod2 :: Int -> Bool
mod2 a = (a `mod` 2) == 0

list = [i * 2 | i <- [1 .. 10], mod2 i] -- list comprehensions

tuple = ("Hi", 10)

dictionary = [("key1", 10), ("key2", 12), ("key3", 13)]

dt :: IO ()
dt = do
  print $ map (+ 2) list
  print $ foldr (+) 0 list
  print $ tuple
  let value = lookup "key1" dictionary
   in case value of
        Just a -> print a
        Nothing -> error "nothing"
