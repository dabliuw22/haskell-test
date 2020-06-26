module TypeClasses (tc) where

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

tc :: IO ()
tc = do
  serviceRun $ A 1
  serviceRun $ B (1, 2)