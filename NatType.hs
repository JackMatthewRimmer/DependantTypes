{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module NatType where
import Data.Kind (Type)

-- Peano Reprisentation of Natual Numbers 
data Nat = Zero | Succ ( Nat )

-- incomplete definition
instance Num Nat where
  n + m = n `natAdd` m
  n * m = n `natMul` m
  abs n = n
  fromInteger = nat 

instance Eq Nat where
  Zero == Zero = True
  (Succ n) == (Succ m) = n == m
  _ == _ = False

instance Ord Nat where
  compare n m | n == m = EQ
              | natLE n m = LT
              | otherwise = GT

instance Show Nat where
  show = show . showNat 

natAdd :: Nat -> Nat -> Nat 
natAdd Zero m = m
natAdd n Zero = n
natAdd (Succ n) (Succ m) = (Succ (Succ n)) `natAdd` m

natMul :: Nat -> Nat -> Nat
natMul Zero _ = Zero
natMul _ Zero = Zero
natMul (Succ n) m = m `natAdd` (n `natMul` m)

natLE :: Nat -> Nat -> Bool
natLE Zero _ = True
natLE (Succ n) Zero = False 
natLE (Succ n) (Succ m) = natLE n m

-- itn == Int to Nat
nat :: Integer -> Nat
nat 0 = Zero
nat n
  | n > 0     = Succ (nat (n - 1))
  | otherwise = error "intToNat is only defined for non-negative integers"

int :: Nat -> Integer -> Integer
int Zero x = x
int (Succ n) x = int n (x+1) 

showNat :: Nat -> Integer
showNat Zero = 0
showNat (Succ n) = int (Succ n) 0 
