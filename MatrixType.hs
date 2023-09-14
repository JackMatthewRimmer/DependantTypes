{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module MatrixType where

import VecType (Vec, Vec(Cons), Vec(Nil))
import NatType (Nat)
import NatType (Nat(Succ), Nat(Zero))

data Matrix :: Nat -> Nat -> * -> * where
  Matrix :: Vec (Vec Int n) m -> Matrix m n a 

mat = Matrix (Cons (Cons 1 Nil) Nil)
mat1 = Matrix (Cons Nil Nil) 
mat2 = Matrix (Cons (Cons 2 (Cons 1 Nil)) Nil)
