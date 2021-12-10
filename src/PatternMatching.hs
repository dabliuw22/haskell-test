module PatternMatching (pm) where

data Expresion
  = Literal Int
  | Negative Expresion
  | Addition Expresion Expresion
  deriving (Eq, Show)

eval :: Expresion -> Int
eval (Literal x) = x
eval (Negative e) = - eval e
eval (Addition a b) = eval a + eval b

view :: Expresion -> String
view (Literal x) = show x
view (Negative e) = "(-" ++ view e ++ ")"
view (Addition a b) = "(" ++ view a ++ " + " ++ view b ++ ")"

pm :: IO ()
pm = do
  let exp = Addition (Negative (Literal 1)) (Literal 3)
  print $ eval exp
  print $ view exp
