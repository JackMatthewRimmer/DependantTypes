{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module VecType where

import NatType (Nat)
import NatType (Nat(Succ), Nat(Zero))

data Vec :: * -> Nat -> * where
  Nil  :: Vec a 'Zero
  Cons :: a -> Vec a n -> Vec a ('Succ n)
