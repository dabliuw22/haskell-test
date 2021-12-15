module Monads where

import Control.Monad ((>=>), (>>), (>>=), Monad, liftM, return)

(<|>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(<|>) m n = m >=> n

(|^) :: Monad m => (a -> b) -> m a -> m b
(|^) f m = liftM f m -- fmap f m

pureMonad :: Monad m => a -> m a
pureMonad = return

func1 :: Int -> IO String
func1 v = return $ show v

func2 :: String -> IO String
func2 = pureMonad

md :: IO ()
md = do
  print "Init md"
    >> ((func1 <|> func2 $ 1) >>= print)
    <* print "End md"
