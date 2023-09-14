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

-- defining num instance however some of the functions will be useless so not defined
instance Num Nat where
  n + m = n `natAdd` m
  n * m = n `natMul` m
  abs n = n
  fromInteger = itn 

instance Eq Nat where
  Zero == Zero = True
  (Succ n) == (Succ m) = n == m
  _ == _ = False

instance Ord Nat where
  compare n m | n == m = EQ
              | natLE n m = LT
              | otherwise = GT


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
itn :: Integer -> Nat
itn 0 = Zero
itn n
  | n > 0     = Succ (itn (n - 1))
  | otherwise = error "intToNat is only defined for non-negative integers"
