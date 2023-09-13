{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

import Data.Kind (Type)

data Nat = Zero | Succ ( Nat )
  deriving (Show)

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n
  | n > 0     = Succ (intToNat (n - 1))
  | otherwise = error "intToNat is only defined for non-negative integers"

data Vec :: * -> Nat -> * where
  Nil  :: Vec a 'Zero
  Cons :: a -> Vec a n -> Vec a ('Succ n)

instance Show a => Show (Vec a n) where
  show = showVec

showVec :: Show a => Vec a n -> String
showVec Nil = "[]"
showVec (Cons x xs) = "[" ++ show x ++ " : " ++ show xs ++ "]" 

vecSum :: Vec Int n -> Vec Int n -> Vec Int n
vecSum (Cons a as) (Cons b bs) = Cons (a + b) (vecSum as bs) 
vecSum Nil Nil = Nil

data Matrix :: Nat -> Nat -> * -> * where
  Matrix :: Nat -> Nat -> Vec (Vec Int n) m -> Matrix n m a 

mat = Matrix (Succ Zero) (Succ Zero) (Cons (Cons 1 Nil) Nil)

-- Define a Show instance for Matrix
instance Show a => Show (Matrix n m a) where
  show (Matrix _ _ rows) = showVec rows