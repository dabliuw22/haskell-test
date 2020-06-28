{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
module JsonTest where

import Data.Aeson
import ClassyPrelude
import GHC.Generics

data Person = Person {
                id :: Int,
                name :: Text
              } deriving (Generic, Show)

instance FromJSON Person
instance ToJSON Person
  --toEncoding = genericToEncoding defaultOptions

data Product = Product {
                serial :: Int,
                stock :: Int
              } deriving Show

instance FromJSON Product where
  parseJSON (Object value) = do
    serial <- value .: "serial"
    stock <- value .: "stock"
    return (Product serial stock)
  parseJSON _ = empty
instance ToJSON Product where
  toJSON (Product serial stock) = object ["serial" .= serial, "stock" .= stock]

main :: IO ()
main = do
  print $ encode (Person 1 "name")
  print $ encode (Product 1 10)
  let v = decode "{\"serial\":2,\"stock\":100}" :: Maybe Product in print v



