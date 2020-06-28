module TypeClasses where

import Data.Char

class Service a where
  run :: a -> (Integer, String)

newtype A = A { value :: Int }

instance Service A where
  run a = let a' = value a in (toInteger a', show a')
  
newtype B = B { tuple :: (Int, Int) }

instance Service B where
  run b = let (b1, b2) = tuple b in (toInteger b1, show b2) 

serviceRun :: Service a => a -> IO ()
serviceRun = print . run

class ServiceWithEffect m where
  execute :: String -> m String

instance ServiceWithEffect IO where
  execute s = pure (map toUpper s)

serviceWithEffectExecute :: ServiceWithEffect m => String -> m String
serviceWithEffectExecute = execute

tc :: IO ()
tc = do
  serviceRun $ A 1
  serviceRun $ B (1, 2)
  a <- serviceWithEffectExecute $ "hello"
  print a
