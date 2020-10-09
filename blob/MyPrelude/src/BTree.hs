{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
module BTree where

import Prelude
import Recursion
import Wrapped

import Data.Ratio
import Data.Traversable
import Data.Functor.Classes
import Data.Monoid
import Data.Zip
import Data.Bifunctor

import Data.Functor.Bind hiding (join)
import Data.Functor.Extend


data BTreeF a f
   = BinF a f f
   deriving
     ( Show, Eq, Ord, Functor
     )

newtype BTree a
      = BTree (Nu (BTreeF a))

pattern Bin a l r
      = Rec (BinF a l r)

type instance Base (BTree a)
      = BTreeF a


deriving newtype
  instance Recursive (BTree a)

deriving newtype
  instance Corecursive (BTree a)

deriving via Ap BTree a
  instance Semigroup a => Semigroup (BTree a)

deriving via Ap BTree a
  instance Monoid a => Monoid (BTree a)

deriving via Ap BTree a
  instance Semiring a => Semiring (BTree a)

deriving via Ap BTree
  instance Functor BTree

deriving via WrappedPoint BTree
  instance Applicative BTree

deriving via WrappedPoint BTree
  instance Comonad BTree

deriving via WrappedPoint BTree
  instance Repeat BTree

deriving via WrappedApplicative BTree
  instance Semialign BTree

deriving via WrappedApplicative BTree
  instance Zip BTree

instance Bifunctor BTreeF where
  bimap f g (BinF a l r)
    = BinF (f a) (g l) (g r)

instance Pointed BTree where
  point a = Bin a (point a) (point a)

instance Copointed BTree where
  copoint (Bin a _ _) = a


instance Apply BTree where
  Bin f fl fr <.> Bin a al ar =
    Bin (f a) (fl <.> al) (fr <.> ar)

instance Extend BTree where
  duplicated =
    ana $ liftA2 first const project

instance Foldable BTree where
  foldMap =
    foldMapDefault

instance Traversable BTree where
  traverse f = transverse
    \case BinF a l r -> liftA3 BinF (f a) l r

-----------

-- Grow a tree using a splitter
grow split lo hi
  = Bin mid (grow split lo mid) (grow split mid hi)
  where
    mid = split lo hi

-- Grow a tree using a ring instance
interval =
  grow \lo hi -> lo + div (hi - lo) 2

-- Like interval, but switch to 'f'
interval' f low high =
  case size of
    0 -> leaf low
    1 -> Bin low (f low) (leaf high)
    2 -> Bin mid (leaf low) (leaf high)
    _ ->
      Bin do mid
          do interval' f low  (mid - 1)
          do interval' f (mid + 1) high
  where
    size   = high - low
    mid    = low + div size 2
    leaf n = Bin n (f n) (f n)

-- Exponential ascent
expFrom =
  expFrom' \case {}

expFrom' f =
  \case
    0 -> Bin 0 (f 0) (go 1)
    n -> go n
  where
    go n =
      let x = n+n
       in Bin do x
              do interval' f (n + 1) (x - 1)
              do go x

-- Search a binary tree using a predicate
bsearch :: (a -> Ordering) -> BTree a -> a
bsearch p
  = \case
  Bin x l r
    -> case p x of
      LT -> bsearch p l
      GT -> bsearch p r
      EQ -> x

-- Farey algorithm to approximate rational numbers
farey lim x = go lim $ fareyGrow (floor x % 1) (ceiling x % 1)
  where
    go 0 (Bin f _ _) = f
    go n (Bin f l r)
      | toRational f == x = f
      | toRational f <  x = go (n-1) r
      | let               = go (n-1) l

fareyGrow :: (Integral a, Semiring a) => Ratio a -> Ratio a -> BTree (Ratio a)
fareyGrow =
  grow \bot top
    -> (numerator bot   + numerator top)
     % (denominator bot + denominator top)

-- O(log n). Integer square root
isqrt n
  = bsearch (compare n . join (*)) $ expFrom 0

