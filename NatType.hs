{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module NatType where
import Data.Kind (Type)

-- Peano Reprisentation of Natual Numbers 
data Nat = Zero | Succ ( Nat )
  deriving (Show)

-- defining num instance however some of the functions will be useless
instance Num Nat where
  n + m = n `natAdd` m
  n * m = n `natMul` m
  abs n = n
  fromInteger = itn 

natAdd :: Nat -> Nat -> Nat 
natAdd Zero m = m
natAdd n Zero = n
natAdd (Succ n) (Succ m) = (Succ (Succ n)) `natAdd` m

natMul :: Nat -> Nat -> Nat
natMul Zero _ = Zero
natMul _ Zero = Zero
natMul (Succ n) m = m `natAdd` (n `natMul` m)

-- itn == Int to Nat
itn :: Integer -> Nat
itn 0 = Zero
itn n
  | n > 0     = Succ (itn (n - 1))
  | otherwise = error "intToNat is only defined for non-negative integers"
