{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenTypeFamily where

import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Word
  ( Word16,
    Word32,
    Word64,
    Word8,
  )
import GHC.TypeLits
  ( KnownSymbol,
    Symbol, -- It lets us use string literals like "hello" as a type.
    symbolVal,
  )

type Label :: Type -> Symbol
type family Label t

type instance Label Double = "number" -- DataKinds

type instance Label String = "string" -- DataKinds

type instance Label Bool = "boolean" -- DataKinds

label :: forall t. KnownSymbol (Label t) => String -- FlexibleContexts, AllowAmbiguousTypes
label = symbolVal (Proxy @(Label t)) -- @ TypeApplications

double = label @Double

type family DoubleSize w

type instance DoubleSize Word16 = Word32

type instance DoubleSize Word32 = Word64

-- Associated
class Encoder a where
  type Result a
  encode :: a -> [Result a]

instance Encoder [a] where
  type Result [a] = a
  encode = id

instance Encoder BS.ByteString where
  type Result BS.ByteString = Word8
  encode = BS.unpack

-- encoder = encode ("Hello" :: String)
-- encoder = encode ([1, 2, 3] :: [Int])
encoder = encode ("Hello" :: BS.ByteString)

{-
class Add a b where
    plus :: a -> b -> ???
-}

class Add a b where -- MultiParamTypeClasses
  type Sum a b
  plus :: a -> b -> Sum a b

instance Add Integer Double where
  type Sum Integer Double = Double
  plus x y = fromIntegral x + y

instance Add Double Integer where
  type Sum Double Integer = Double
  plus x y = x + fromIntegral y

instance (Num a) => Add a a where -- FlexibleInstances
  type Sum a a = a
  plus x y = x + y

add = plus (5 :: Integer) (6 :: Double)
