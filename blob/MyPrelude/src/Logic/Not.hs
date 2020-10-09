{-# LANGUAGE TupleSections #-}
module Logic.Not where

import Base
import Data.Functor.Contravariant


newtype Not a
      = Not { elimNot :: a -> ∀ b. b }

instance Contravariant Not where
  contramap ab (Not b)
    = Not (b . ab)

-----

contradicts :: a -> Not a -> b
contradicts = flip elimNot

noncontra :: Not (a, Not a)
noncontra = Not $ uncurry contradicts

notVoid :: Not Void
notVoid = Not absurd

nor :: Not a -> Not b -> Not (Either a b)
nor a b = Not $ either (elimNot a) (elimNot b)

notNot :: a -> Not (Not a)
notNot a = contramap (a,) noncontra

