{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
module BTree where

import Prelude
import Recursion
import Wrapped
import Utils

import Control.Monad.Free.Church
import Control.Monad.Trans.Free as CMTF

import Data.Functor.Classes
import Data.Functor.Compose
import Data.Semigroup.Foldable.Class
import Data.Bifunctor
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Monoid
import Data.Ratio
import Data.Traversable
import Data.Zip
import Unsafe.Coerce

import Data.Functor.Bind hiding (join)
import Data.Functor.Extend


data BTreeF a f
   = BinF a f f
   deriving
     ( Show, Eq, Ord
     , Functor, Foldable, Traversable
     )

newtype BTree a
      = BTree (Nu (BTreeF a))

{-# COMPLETE Bin #-}

pattern Bin ∷ a → BTree a → BTree a → BTree a
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

instance Functor BTree where
  fmap f = \case
    Bin a l r → Bin (f a) (fmap f l) (fmap f r)

  a <$ _ =
    point a

-- deriving via Ap BTree
--   instance Functor BTree

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

instance Pointed BTree where
  point a = Bin a (point a) (point a)

instance Copointed BTree where
  copoint (Bin a _ _) = a

instance Apply BTree where
  _ .> b = b
  a <. _ = a

  Bin f fl fr <.> Bin a al ar =
    Bin (f a) (fl <.> al) (fr <.> ar)

  liftF2 f (Bin la ll lr) (Bin ra rl rr) =
    Bin (f la ra) (liftF2 f ll rl) (liftF2 f lr rr)

instance Extend BTree where
  duplicated =
    ana $ liftF2 first const project

instance Foldable BTree where
  foldMap =
    foldMapDefault

instance Traversable BTree where
  traverse f = transverse
    \case BinF a l r -> liftA3 BinF (f a) l r

-----------

instance Bifunctor BTreeF where
  bimap
    = bimapDefault

instance Bifoldable BTreeF where
  bifoldMap
    = bifoldMapDefault

instance Bitraversable BTreeF where
  bitraverse f g (BinF a l r)
    = liftA3 BinF (f a) (g l) (g r)

instance Biapplicative BTreeF where
  bipure a b
    = BinF a b b

  BinF fx fl fr <<*>> BinF x l r
    = BinF (fx x) (fl l) (fr r)


instance Eq2 BTreeF where
  liftEq2 fl fr (BinF la lb lc) (BinF ra rb rc)
    = fl la ra && fr lb rb && fr lc rc

instance Ord2 BTreeF where
  liftCompare2 fl fr (BinF la lb lc) (BinF ra rb rc)
    = fl la ra <> fr lb rb <> fr lc rc

instance Eq a => Eq1 (BTreeF a) where
  liftEq
    = liftEq2 (==)

instance Ord a => Ord1 (BTreeF a) where
  liftCompare
    = liftCompare2 compare

-----------

newtype BTreeTF m a f
   = BinTF (m (BTreeF a f))
   deriving
     ( Functor, Foldable, Traversable
     )

newtype BTreeT m a
      = BTreeT (Nu (BTreeTF m a))



type instance Base (BTreeT m a)
      = BTreeTF m a

-- 'unsafeCoerce' is to avoid 'fmap coerce'
-- Alternatively we could require 'Paramatrically Coercible m'
instance Functor m => Recursive (BTreeT m a) where
  project (BTreeT a)
    = unsafeCoerce (project a)

instance Functor m => Corecursive (BTreeT m a) where
  embed = ana (fmap project)
  ana f = coerce #. Nu f


instance Functor m => Functor (BTreeT m) where
  fmap :: ∀ a b. Functor m => (a -> b) -> BTreeT m a -> BTreeT m b
  fmap f (BTreeT (Nu g a))
    = BTreeT $ Nu (go . g) a
    where
      go (BinTF a) = BinTF $ fmap (first f) a

instance Pointed m => Pointed (BTreeT m) where
  point = BTreeT #. Nu f
    where
      f a = BinTF #. point $ bipure a a

instance Apply m => Apply (BTreeT m) where
  (project->BinTF l) <.> (project->BinTF r)
    = embed $ BinTF $ liftF2 f l r
    where
      f (BinF fx fl fr) (BinF x l r)
        = BinF (fx x) (fl <.> l) (fr <.> r)

deriving via WrappedPoint (BTreeT m)
  instance (Pointed m, Apply m) => Applicative (BTreeT m)


instance (Functor m, Foldable m) => Foldable (BTreeT m) where
  foldMap am (project->BinTF m) =
    flip foldMap m \(BinF a l r) →
      foldMap am l <> am a <> foldMap am r

-- instance Traversable m => Traversable (BTreeT m) where
--   traverse f (project->BinTF m) =
--     undefined
--     where
--       x f t@(BinF _ _ _) = traverse f t
--       y = x f <$> m

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
isqrt' n
  = bsearch (compare n . join (*)) $ expFrom 0

-------

bsearchT :: Monad m => (a -> Ordering) -> BTreeT m a -> m a
bsearchT p (project->BinTF ma) = do
  BinF x l r <- ma
  case p x of
    LT -> bsearchT p l
    GT -> bsearchT p r
    EQ -> pure x

binT :: (Functor m, Pointed m) => a -> BTreeT m a -> BTreeT m a -> BTreeT m a
binT a l r
  = embed $ BinTF $ point $ BinF a l r

type BE l r = BTreeT (Either l) r

bLeft :: l -> BE l r
bLeft = embed . BinTF . Left

pattern BL ∷ l → BE l r
pattern BL a <- (project->BinTF (Left a))
  where BL = bLeft

pattern BR ∷ r → BE l r → BE l r → BE l r
pattern BR a l r <- (project->BinTF (Right (BinF a l r)))
  where BR = binT


intervalT f low high =
  case size of
    0 -> leaf low
    1 -> binT low (f low) (leaf high)
    2 -> binT mid (leaf low) (leaf high)
    _ ->
      binT do mid
           do intervalT f low  (mid - 1)
           do intervalT f (mid + 1) high
  where
    size   = high - low
    mid    = low + div size 2
    leaf n = binT n (f n) (f n)

safeIntervalT f a b
  = intervalT f (min a b) (max a b)

expFromT f =
  \case
    0 -> binT 0 (f 0) (go 1)
    n -> go n
  where
    go n =
      let x = n+n
       in binT do x
               do safeIntervalT f (n + 1) (x - 1)
               do go x
isqrt
  = hunt (join (*))

hunt f n
  = either id id
  . bsearchT (compare n . f)
  . flip expFromT 0
  $ bLeft
