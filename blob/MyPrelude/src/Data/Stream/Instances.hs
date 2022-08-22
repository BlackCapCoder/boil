{-# LANGUAGE BangPatterns, MagicHash #-}
module Data.Stream.Instances where

import GHC.Exts
import Data.Semigroup


import Prelude hiding
  ( head, tail, scanl, scanl1, scanr, scanr1
  , cycle, transpose, iterate, (!!), filter
  , catMaybes, mapMaybe, zipWith, drop
  )
import Data.Stream.Internal
  ( foldrS, foldr1S, foldrLazy, mapS
  , foldr2, foldr3
  , head, tail, uncons, init, filter
  , iterate, repeat, any, all, elem
  , zipWith3, unzip3, (!!), unfoldrS
  , elemIndex, find
  )
import qualified Data.Stream.Internal as U
import Data.Stream.Type
import BTree (isqrt)


------

instance Apply Stream where
  _ .> a = a
  a <. _ = a

  (f :- fs) <.> (a :- as) =
    f a :- (fs <.> as)

  liftF2 abc (a:-as) (b:-bs) =
    abc a b :- liftF2 abc as bs

instance Applicative Stream where
  pure   = point
  liftA2 = liftF2
  (<*>)  = (<.>)
  ( *>)  = ( .>)
  (<* )  = (<. )

instance Monad Stream where
  (>>=) = (>>-)
  (>> ) = (.> )

instance Semigroup (Stream a) where
  (<>) = interleave

  stimes n0 s0 | n0 <= 1 = s0
  stimes (fi->I# n0) s0 = go s0 where
    go (a:-as) = repl n0 a (go as)
    repl 0# _ as = as
    repl n  a as = a :- repl (n -# 1#) a as

instance Bind Stream where
  {-# INLINE join #-}
  join = diag'

instance Traversable Stream where
  traverse f ~(a :- as) =
    (:-) <$> f a <*> traverse f as

instance Foldable Stream where
  foldr f _ = foldrS f
  foldr1 = foldr1S
  null _ = False

instance Extend Stream where
  duplicated = codiag

instance Comonad Stream where
  duplicate = duplicated
  extend    = extended
  extract   = copoint

instance Eq a => Eq (Stream a) where
  (==) a b =
    and $ U.zipWith (==) a b

  (/=) a b =
    and $ U.zipWith (/=) a b

instance Ord a ⇒ Ord (Stream a) where
  compare (a:-as) (b:-bs) =
    compare a b <> compare as bs

----------

-- id = uncurry interleave . uninterleave

interleave (a:-as) (b:-bs) =
  a :- b :- interleave as bs

uninterleave xs
  = (evens xs, odds xs)
  where
    evens (a:-_:-as) = a :- evens as
    odds  (_:-a:-as) = a :- odds  as

----------

transpose ~((x :- xs) :- yss) =
  (x :- (copoint <$> yss)) :- transpose (xs :- (tail <$> yss))

------

-- a0 :-
-- a1 :- b0 :-
-- a2 :- b1 :- c0 :-
-- ...
{-# INLINE [0] diag #-}
diag =
  unfoldrS \case
    ((!x):-xs):-ys ->
      (x, U.zipWith (:-) xs ys)

{-# INLINE [0] diag' #-}
diag' :: Stream (Stream a) -> Stream a
diag' s =
  mapS (diagIxI s) nats

{-# INLINE [0] codiag #-}
codiag :: Stream a -> Stream (Stream a)
codiag s =
  mapS (index s) $
    nats & mapS \i ->
    nats & mapS \j ->
      pro π (j, i)

{-# RULES

"duplicate/join" forall a. diag  (codiag a) = a
"duplicate/join" forall a. diag' (codiag a) = a
"duplicate/join" forall a. codiag (diag  a) = a
"duplicate/join" forall a. codiag (diag' a) = a

  #-}


diagIx s (con π->(i,j)) =
  s !! j !! i

diagIxI s (con π->(i,j)) =
  s `indexI` j `indexI` i

----

-- alternative duplicate instance
--
tails w@(_:-as) =
  w :- tails as

-- alternative extend instance
--
tailsmap f w@(_:-as) =
  f w :- tailsmap f as

-- no obvious `join` instance such that `join.duplicate = id`,
-- hence `diag/codiag`.

----

drop 0 a = a
drop n (_:-xs) = drop (n-1) xs

index s is = go 0 is s
  where
    go i (j:-js) (drop (j-i)->a:-as)
      = a :- go (j+1) js as

----------

-- nats :: Stream Int
-- nats = iterate (+1) 0

nats = go 0# where
  go !n = I# n :- go (n +# 1#)

-- π ∷ Iso (->) (Int, Int) Int
π = Iso pro con
  where
    pro (x,y) = div ((x+y)*(x+y+1)) 2 + y
    con (z)   = (x, y)
      where
        w = div (isqrt (8*z+1)-1) 2
        t = div (w*w + w) 2
        y = z - t
        x = w - y

----

dropI s0 (I# i) = go s0 (negateInt# i) where
  go s 0# = s
  go (_:-s) n = go s (n +# 1#)

indexI s = copoint . dropI s

