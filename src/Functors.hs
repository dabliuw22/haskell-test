module Functors where

data Data = Data String Int String deriving Show

string :: Applicative f => String -> f String
string = pure
int :: Applicative f => Int -> f Int
int = pure

ft :: IO ()
ft = do
  d <- Data <$> string "def" <*> int 1 <*> string "val"
  print d
