module Functions (fun) where

plus10 :: Int -> Int
plus10 i = i + 10

compose = plus10 . \x -> x - 5

plus :: Int -> Int -> Int
plus a b = a + b

left = (4 `plus`)

right = (`plus` 10)

addition :: Num a => [a] -> a
-- addition (h: t) = h + (foldr (+) 0 t) -- addition list = head list + sum (tail list)
-- addition _ = 0 -- addition [] = 0
addition list = case list of
  (h : t) -> h + sum t
  (_) -> 0

myMap :: (a -> b) -> [a] -> [b]
myMap f (head : tail) = f head : myMap f tail
myMap _ [] = []

fun :: IO ()
fun = do
  let cur = plus 1
  print $ addition [1 .. 3]
  print $ compose 2
  print $ 1 `plus` 2
  print $ cur 2
  print $ left 10
  print $ right 4
  print $ myMap (+ 2) [1 .. 3]
  print $ myMap ((\s -> "(" ++ s ++ ")") . show) [1, 2, 3]
