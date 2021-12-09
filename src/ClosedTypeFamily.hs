{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Type families permit a program to compute what data constructors it will operate on.
-- Closed type families: to implement operations on GADTs.
module ClosedTypeFamily where

import Data.Kind (Type)
import Text.Show.Functions

type HList :: [Type] -> Type -- StandaloneKindSignatures
data HList xs where -- GADTs
  HNil :: HList '[]
  (:::) :: x -> HList xs -> HList (x : xs) -- TypeOperators

infixr 5 :::

h1 :: HList [Integer, String, Bool]
h1 = 42 ::: "Hello" ::: True ::: HNil

h2 :: HList [Bool, Integer]
h2 = False ::: 1 ::: HNil

hlength :: HList xs -> Int
hlength HNil = 0
hlength (_ ::: xs) = 1 + hlength xs

-- happend :: HList xs -> HList ys -> HList ??

type Append :: forall a. [a] -> [a] -> [a] -- kind signature PolyKinds and StandaloneKindSignatures
type family Append xs ys where -- header
  Append '[] ys = ys -- clause 1
  Append (x : xs) ys = x : Append xs ys -- clause 2

happend :: HList xs -> HList ys -> HList (Append xs ys)
happend HNil ys = ys
happend (x ::: xs) ys = x ::: happend xs ys

h3 :: HList (Append [Integer, String, Bool] [Bool, Integer]) -- DataKinds
h3 = happend h1 h2
