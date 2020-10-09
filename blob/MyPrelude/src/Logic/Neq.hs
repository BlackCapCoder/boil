{-# LANGUAGE PolyKinds #-}
module Logic.Neq where

import Base
import Logic.Not
import Data.Type.Equality
import Data.Functor.Contravariant


newtype a != b = Neq { unNeq :: Not (a :~: b) }


refl :: a != b -> b != a
refl = neqMap sym

neqTrans :: (b != c) -> (a :~: b) -> (a != c)
neqTrans bc Refl = bc

neqMap :: (a :~: b -> c :~: d) -> (c != d) -> (a != b)
neqMap f = Neq . contramap f . unNeq

neqAbsurd :: (a != a) -> b
neqAbsurd = contradicts id . unNeq

neqInner :: f :~: g -> f a != g b -> a != b
neqInner = neqMap . apply

neqOuter :: a :~: b -> f a != g b -> f != g
neqOuter = neqMap . flip apply

neqInside :: a != b -> f a != f b
neqInside = neqMap inner

neqAround :: f != g -> f a != g b
neqAround = neqMap outer

