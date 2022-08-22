{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
module Iso where

import Base
import Control.Applicative
import Control.Category
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor
import Data.Bifunctor.Apply
import Data.Bifunctor.Functor
import Data.Bitraversable
import Data.Copointed
import Data.Functor.Apply
-- import Data.Functor.Invariant
import Data.Groupoid
import Data.Monoid
import Data.Pointed
import Data.Semigroup
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Semigroupoid
import Data.Semiring
import qualified Control.Arrow as Arr



type Iso :: (k -> k -> Type) -> k -> k -> Type
data Iso k a b
   = Iso { pro :: k a b, con :: k b a }

type (<->) =
  Iso (->); infixr 0 <->


swapIso (Iso a b) =
  Iso b a


instance Category k => Category (Iso k)
  where
    id = Iso id id
    Iso bc cb . Iso ab ba
      = (bc . ab) `Iso` (ba . cb)

instance Semigroupoid k => Semigroupoid (Iso k)
  where
    Iso bc cb `o` Iso ab ba
      = (bc `o` ab) `Iso` (ba `o` cb)

instance Groupoid k => Groupoid (Iso k)
  where
    inv =
      swapIso

instance BifunctorFunctor Iso
  where
    bifmap f (Iso a b) =
      Iso (f a) (f b)

instance (Semigroup (k a b), Semigroup (k b a)) => Semigroup (Iso k a b)
  where
    Iso ll lr <> Iso rl rr
      = (ll <> rl) `Iso` (lr <> rr)

instance (Monoid (s a b), Monoid (s b a)) => Monoid (Iso s a b)
  where
    mempty = mempty `Iso` mempty

-- instance (Abelian (k a b), Abelian (k b a)) => Abelian (Iso k a b)

-- instance (Semiring (k a b), Semiring (k b a)) => Semiring (Iso k a b) where
--   zero  = Iso zero zero
--   one   = Iso one  one
--   plus  = isozip plus  plus
--   times = isozip times times
-- isozip fl fr (Iso la ra) (Iso lb rb)
--   = Iso (fl la lb) (fr ra rb)

instance (Monoid (k a a), Category k) => Semiring (Iso k a a) where
  zero  = mempty
  plus  = (<>)
  times = (.)
  one   = id

instance (Arr.Arrow k, Groupoid k) => Arr.Arrow (Iso k)
  where
    first (Iso f g)
      = Iso (Arr.first f) (Arr.first g)

    arr f | g <- Arr.arr f
      = Iso g (inv g)

instance Bifunctor k => Bifunctor (Iso k) where
  first ab (Iso l r)
    = Iso (first ab l) (second ab r)
  second f (Iso l r)
    = Iso (second f l) (first f r)
  bimap ab cd (Iso ac ca)
    = Iso (bimap ab cd ac) (bimap cd ab ca)

instance BifunctorComonad Iso where
  biextract (Iso f g)
    = f
  biduplicate (Iso f g)
    = Iso (Iso f g) (Iso g f)

instance Bifoldable k => Bifoldable (Iso k) where
  bifoldMap f g (Iso a b)
    = bifoldMap f g a <> bifoldMap g f b

instance Bifoldable1 k => Bifoldable1 (Iso k) where
  bifoldMap1 f g (Iso a b)
    = bifoldMap1 f g a <> bifoldMap1 g f b

instance Bitraversable k => Bitraversable (Iso k) where
  bitraverse f g (Iso a b)
    = liftA2 Iso (bitraverse f g a) (bitraverse g f b)

instance Bitraversable1 k => Bitraversable1 (Iso k) where
  bitraverse1 f g (Iso a b)
    = liftF2 Iso (bitraverse1 f g a) (bitraverse1 g f b)

instance Biapplicative k => Biapplicative (Iso k) where
  bipure a b
    = Iso (bipure a b) (bipure b a)
  Iso f g <<*>> Iso h k
    = Iso (f <<*>> h) (g <<*>> k)
  biliftA2 f g (Iso a b) (Iso c d)
    = Iso (biliftA2 f g a c) (biliftA2 g f b d)

instance Biapply k => Biapply (Iso k) where
  Iso f g <<.>> Iso h k
    = Iso (f <<.>> h) (g <<.>> k)

instance Bifunctor k => Functor (Iso k a) where
  fmap = second

instance (Pointed (k a), Groupoid k) => Pointed (Iso k a) where
  point a | p <- point a = Iso p (inv p)

instance (Copointed (k a)) => Copointed (Iso k a) where
  copoint (Iso a b) = copoint a

-----


