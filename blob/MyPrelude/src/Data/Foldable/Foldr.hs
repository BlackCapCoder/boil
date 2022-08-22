{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
module Data.Foldable.Foldr where

import Base
import Data.Foldable.Unfoldable
import Data.Foldable.Hyp
import Unit
import Wrapped
import qualified GHC.Exts as GHC

newtype Foldr a
      = Foldr { runFoldr :: ∀ b. (a -> b -> b) -> b -> b }


instance Foldable Foldr where
  foldr f e (Foldr foldr) = foldr f e

instance Unfoldable Foldr where
  unfold t = Foldr \f z -> foldr f z t


cons :: a -> Foldr a -> Foldr a
cons a (Foldr f) = Foldr
  \cons q -> cons a (f cons q)

snoc :: a -> Foldr a -> Foldr a
snoc a (Foldr f) = Foldr
  \cons q -> f cons (cons a q)

tail :: Foldr a -> Foldr a
tail (Foldr f) = Foldr \cons q ->
  f (\x rst g -> g x (rst cons)) (\_ -> q) \_ xs -> xs

uncons t =
  (, tail t) <$> safeHead t

pattern (:-) x xs <- (uncons->Just (x,xs))
  where (:-) x xs = cons x xs

----

deriving stock
  instance Functor Foldr

deriving via WrappedPoint Foldr
  instance Applicative Foldr

deriving via WrappedPoint Foldr
  instance Monad Foldr


instance Unit (Foldr a) where
  unit = Foldr
    \_ e -> e

instance Monoid (Foldr a) where
  mempty =
    unit

instance Pointed Foldr where
  point a =
    cons a unit

instance Semigroup (Foldr a) where
  Foldr a <> Foldr b =
    Foldr (liftA2 (.) a b)

instance Apply Foldr where
  -- (<.>) = go where
  --   go (f:-fs) (a:-as) = f a :- go fs as
  --   go _       _       = mempty

  liftF2 = zipWith
  -- liftF2 f = go where
  --   go (a:-as) (b:-bs) = f a b :- go as bs
  --   go _       _       = mempty

instance Bind Foldr where
  join  = fold
  (>>-) = flip foldMap

instance Semialign Foldr where
  alignWith f (x:-xs) (y:-ys) = f (These x y) :- alignWith f xs ys
  alignWith f (x:-xs) _       = f (This  x  ) :- fmap (f . This) xs
  alignWith f _       (y:-ys) = f (That    y) :- fmap (f . That) ys
  alignWith f _       _       = mempty

instance Zip Foldr where
  zipWith f (Foldr l) (Foldr r) = Foldr \cons nil →
    case () of
      () → l f1 nilL `invoke` r f2 nilR
        where
          f1 x xk = H \yk → invoke yk xk x
          f2 y yk = H \xk x → f x y `cons` invoke xk yk

          nilL = H \_ → nil
          nilR = H \_ _ → nil

instance Unzip Foldr where
  unzip =
    unzipDefault

instance Traversable Foldr where
  traverse f = go where
    go (x:-xs) = liftA2 cons (f x) (go xs)
    go _       = pure mempty

instance GHC.IsList (Foldr a) where
  type Item (Foldr a) = a

  toList   = unfold
  fromList = unfold

instance Show a => Show (Foldr a) where
  show = show . toList


