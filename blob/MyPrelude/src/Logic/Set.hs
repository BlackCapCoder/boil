{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Logic.Set where

import Base
import Iso

import Data.Semigroupoid
import Data.Bifunctor.Functor


type a ∈ b
   = b a

type SET
   = Type -> Type


type SetEq k
   = Iso (Subset k)

type    Subset :: (x -> x -> Type) -> (a -> x) -> (a -> x) -> Type
newtype Subset k f g
      = Subset { unsub :: ∀ a. (a ∈ f) `k` (a ∈ g) }

newtype Empty e
      = Empty { elimEmpty :: (∀ a. a) }

data Univ a = Univ


------

instance Category k => Category (Subset k)
  where
    id                    = Subset id
    Subset bc . Subset ab = Subset (bc . ab)

instance Semigroupoid k => Semigroupoid (Subset k)
  where
    Subset bc `o` Subset ab = Subset (bc `o` ab)

instance BifunctorFunctor Subset
  where
    bifmap f (Subset a) = Subset (f a)


