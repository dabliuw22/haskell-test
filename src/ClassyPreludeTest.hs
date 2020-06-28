{-# LANGUAGE NoImplicitPrelude #-}
module ClassyPreludeTest where

import Data.Time
import ClassyPrelude
--import Prelude hiding (print)

f :: String -> Text
f = fromString

--dict :: Map Text Int
dict = [("key1", 1), ("key2", 2)]

main :: IO ()
main = do 
  print $ f "Hello"
  time <- getCurrentTime
  print time
