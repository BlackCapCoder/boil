{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
module BTree where

import Prelude
import Data.Ratio
import Data.Traversable


data BTree a
   = Bin a (BTree a) (BTree a)
   deriving
     ( Show, Eq, Ord, Functor
     )

instance Pointed BTree where
  point a =
    Bin a (pure a) (pure a)

instance Copointed BTree where
  copoint (Bin a _ _) =
    a

instance Applicative BTree where
  pure =
    point

  Bin f fl fr <*> Bin a al ar =
    Bin (f a) (fl <*> al) (fr <*> ar)

instance Comonad BTree where
  extract =
    copoint

  duplicate b@(Bin _ l r) =
    Bin b (duplicate l) (duplicate r)

instance Foldable BTree where
  foldMap =
    foldMapDefault

instance Traversable BTree where
  traverse f (Bin a l r) =
    liftA3 Bin (f a) (traverse f l) (traverse f l)

instance Semigroup a => Semigroup (BTree a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (BTree a) where
  mempty = pure mempty

instance Semiring a => Semiring (BTree a) where
  zero  = pure zero
  one   = pure one
  plus  = liftA2 plus
  times = liftA2 times

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
  expFrom' undefined

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
bsearch :: Ord a => (a -> Ordering) -> BTree a -> a
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

fareyGrow =
  grow \bot top
    -> (numerator bot   + numerator top)
     % (denominator bot + denominator top)

-- O(log n). Integer square root
isqrt n
  = bsearch (compare n . join (*)) $ expFrom 0

