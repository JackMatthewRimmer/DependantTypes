{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module NatType where
import Data.Kind (Type)


data Nat = Zero | Succ ( Nat )
  deriving (Show)

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n
  | n > 0     = Succ (intToNat (n - 1))
  | otherwise = error "intToNat is only defined for non-negative integers"
