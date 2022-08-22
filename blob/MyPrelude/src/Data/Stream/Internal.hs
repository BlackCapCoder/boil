{-# LANGUAGE BangPatterns #-}
module Data.Stream.Internal where


import Data.Stream.Type
import Prelude hiding
  ( head, tail, scanl, scanl1, scanr, scanr1
  , cycle, transpose, iterate, (!!), filter
  , catMaybes, mapMaybe, init, repeat
  , any, all, elem, elemIndex
  , zip, zip3, zipWith, zipWith3, unzip, unzip3
  , find
  )

import qualified Data.Zip as Z

----------
-- Right folds

foldrLazy :: (a -> b -> b) -> Stream a -> b
foldrLazy f = go where
  go ~(a:-as) = f a (go as)

foldr1S :: (a -> b -> b) -> Stream a -> b
foldr1S f (x:-xs) = go x xs where
  go !acc (a :- as)
    = f acc (go a as)

-----

{-# NOINLINE nil #-}

nil :: ∀ a. Stream a
nil = error "stream nil"


{-# INLINE [0] foldrS #-}

foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f = go where
  go ((:-) !a !as) = f a (go as)


{-# INLINE [0] foldrz #-}

foldrz :: (a -> b -> b) -> z -> Stream a -> b
foldrz f _ = go where
  go ((:-) !a !as) = f a (go as)


{-# INLINE [1] build #-}

build :: ∀ a. (∀ b. (a -> b -> b) -> b) -> Stream a
build g = g (:-)


{-# INLINE [1] buildz #-}

buildz :: ∀ a. (∀ b. (a -> b -> b) -> b -> b) -> Stream a
buildz g = g (:-) nil


{-# INLINE [1] augment #-}

augment :: ∀ a. (∀ b. (a -> b -> b) -> b -> b) -> Stream a -> Stream a
augment g xs = g (:-) xs



{-# RULES
"fold/build"     forall k (g::forall b. (a->b->b) -> b) .
                 foldrS k (build g) = g k

"fold/buildz"  forall k (g::forall b. (a->b->b) -> b -> b) .
                 foldrS k (buildz g) = g k nil

"foldrS/augment" forall k xs (g::forall b. (a->b->b) -> b -> b) .
                 foldrS k (augment g xs) = g k (foldrS k xs)

"foldrz/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) .
                 foldrz k z (augment g xs) = g k (foldrz k z xs)

"foldrS/id"      forall x. foldrS (:-) x = x
"foldrz/id"                foldrz (:-) nil = \x -> x

"foldrS/cons/build"  forall k x (g::forall b. (a->b->b) -> b) .
                     foldrS k (x :- build g) = k x (g k)

"foldrz/cons/build"  forall k z x (g::forall b. (a->b->b) -> b -> b) .
                     foldrz k z (x :- buildz g) = k x (g k z)

"augment/buildz" forall (g::forall b. (a->b->b) -> b -> b)
                          (h::forall b. (a->b->b) -> b -> b) .
                          augment g (buildz h) = buildz (\c n -> g c (h c n))

"augment/build" forall (g::forall b. (a->b->b) -> b -> b)
                       (h::forall b. (a->b->b) -> b) .
                       augment g (build h) = build (\c -> g c (h c))

-- -- "foldr/cons" forall k x xs. foldrS k (x:-xs) = k x (foldrS k xs)


"foldrz/nil"    forall k z.
                foldrz k z nil = z

"augment/nil"   forall (g::forall b. (a->b->b) -> b -> b) .
                augment g nil = buildz g

-- "foldr/cons" forall k z x xs. foldr k z (x:xs) = k x (foldr k z xs)
-- "foldr/single"  forall k z x. foldr k z [x] = k x z

 #-}


----------------------


{-# NOINLINE [0] mapS #-}
mapS :: (a -> b) -> Stream a -> Stream b
mapS f (a:-as) = f a :- mapS f as
-- mapS f = go where
--   go (a:-as) = f a :- go as


{-# INLINE [0] mapFB #-} -- See Note [Inline FB functions] in GHC.List
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
mapFB c f = \x ys -> c (f x) ys


{-# RULES

"mapS"      [~1] forall f xs.   mapS f xs  = build (\c -> foldrS (mapFB c f) xs)
"mapStream" [1]  forall f.      foldrS (mapFB (:-) f)   = mapS f
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
"mapFB/id"  forall c.           mapFB c (\x -> x)       = c

  #-}

{-# RULES "mapS/coerce" [1] mapS coerce = coerce #-}


-------------

instance Copointed Stream where
  {-# INLINE copoint #-}
  copoint = head

instance Pointed Stream where
  {-# INLINE point #-}
  point = repeat

instance Functor Stream where
  {-# INLINE fmap #-}
  fmap = mapS

  {-# INLINE (<$) #-}
  a <$ _ = repeat a

-----------

{-# NOINLINE head #-}
head (a:-_) = a

{-# INLINE tail #-}
tail (_:-a) = a

{-# RULES
"head/build"    forall (g::forall b.(a->b->b)->b) .
                head (build g) = g (\x _ -> x)
"head/augment"  forall xs (g::forall b. (a->b->b) -> b -> b) .
                head (augment g xs) = g (\x _ -> x) (head xs)
 #-}


{-# INLINE uncons #-}
uncons(a:-as)=(a,as)

{-# INLINE init #-}
init :: Stream a -> Stream a
init = id


{-# NOINLINE [1] filter #-}
filter :: (a -> Bool) -> Stream a -> Stream a
filter p (x:-xs)
  | p x = x :- filter p xs
  | let = filter p xs


{-# INLINE [0] filterFB #-}
filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
filterFB c p x r | p x       = x `c` r
                 | otherwise = r


{-# RULES
"filter"     [~1] forall p xs.  filter p xs = build (\c -> foldrS (filterFB c p) xs)
"filterList" [1]  forall p.     foldrS (filterFB (:-) p) = filter p
"filterFB"        forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
 #-}


{-# NOINLINE [1] mapMaybe #-}
mapMaybe f ~(a :- as) =
  case f a of
    Just a' -> a' :- mapMaybe f as
    Nothing -> mapMaybe f as


{-# NOINLINE [1] catMaybes #-}
catMaybes :: Stream (Maybe a) -> Stream a
catMaybes ~(a :- as) =
  case a of
    Just a' -> a' :- catMaybes as
    Nothing -> catMaybes as


{-# INLINE [0] mapMaybeFB #-}
mapMaybeFB :: (a -> b -> b) -> (a -> Maybe a) -> a -> b -> b
mapMaybeFB c p x r | Just a<-p x = a `c` r
                   | otherwise   = r

{-# INLINE [0] catMaybesFB #-}
catMaybesFB :: (a -> b -> b) -> Maybe a -> a -> b -> b
catMaybesFB c p x r | Just a<-p = a `c` r
                    | otherwise = r

{-# RULES
"mapMaybe"   [~1] forall p xs. mapMaybe p xs = build (\c -> foldrS (mapMaybeFB c p) xs)
"mapMaybeList" [1]  forall p.     foldrS (mapMaybeFB (:-) p) = mapMaybe p
"mapMaybeFB"       forall c p q. mapMaybeFB (mapMaybeFB c p) q =
                        mapMaybeFB c (\x -> q x *> p x)

-- "catMaybes"   [~1] forall p xs. catMaybes p xs = build (\c -> foldrS (catMaybesFB c p) xs)
-- "catMaybesList" [1]  forall p.     foldrS (catMaybesFB (:-) p) = catMaybes p
-- "catMaybesFB"       forall c p q. mapMaybeFB (mapMaybeFB c p) q =
                        -- mapMaybeFB c (\x -> q x *> p x)
 #-}



{-# NOINLINE [1] iterate #-}
iterate :: (a -> a) -> a -> Stream a
iterate f x = x :- iterate f (f x)

{-# INLINE [0] iterateFB #-} -- See Note [Inline FB functions]
iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f x0 = go x0
  where go x = x `c` go (f x)

{-# RULES
"iterate"    [~1] forall f x.   iterate f x = build (\c -> iterateFB c f x)
"iterateFB"  [1]                iterateFB (:-) = iterate
 #-}


{-# INLINE [0] repeat #-}
repeat :: a -> Stream a
repeat x = xs where xs = x :- xs

{-# INLINE [0] repeatFB #-}
repeatFB :: (a -> b -> b) -> a -> b
repeatFB c x = xs where xs = x `c` xs

{-# RULES
"repeat"    [~1] forall x. repeat x = build (\c -> repeatFB c x)
"repeatFB"  [1]  repeatFB (:-)      = repeat
 #-}

---

any :: (a -> Bool) -> Stream a -> Bool
any p (x:-xs) = p x || any p xs

{-# NOINLINE [1] any #-}
{-# RULES
"any/build"     forall p (g::forall b.(a->b->b)->b) .
                any p (build g) = g ((||) . p)
 #-}


all :: (a -> Bool) -> Stream a -> Bool
all p (x:-xs) =  p x && all p xs

{-# NOINLINE [1] all #-}
{-# RULES
"all/build"     forall p (g::forall b.(a->b->b)->b) .
                all p (build g) = g ((&&) . p)
 #-}


elem x (y:-ys) = x==y || elem x ys

{-# NOINLINE [1] elem #-}
{-# RULES
"elem/build"    forall x (g :: forall b . Eq a => (a -> b -> b) -> b)
   . elem x (build g) = g (\y r -> (x == y) || r)
 #-}


{-# INLINABLE (!!) #-}
xs !! n
  | n < 0     = negIndex
  | otherwise = foldrS (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) xs n

negIndex =
  errorWithoutStackTrace "!! negative index"


elemIndex x = go 0
  where
    go n (a:-as)
      | x == a    = n
      | otherwise = go (n+1) as


find :: (a -> Bool) -> Stream a -> a
find f = go
  where
    go (a:-as)
      | f a = a
      | let = go as

---------------


{-# INLINE [0] foldr2 #-}
foldr2 :: (a -> b -> c -> c) -> Stream a -> Stream b -> c
foldr2 k = go where
  go (x:-xs) (y:-ys) = k x y (go xs ys)

foldr2_left :: (a -> b -> c -> d) -> a -> (Stream b -> c) -> Stream b -> d
foldr2_left  k x r (y:-ys) = k x y (r ys)

-- foldr2 k z xs ys = foldr (foldr2_left k z)  (\_ -> z) xs ys
{-# RULES
"foldr2/left"   forall k ys (g::forall b.(a->b->b)->b) .
                foldr2 k (build g) ys = g (foldr2_left  k) ys
 #-}


{-# INLINE [0] foldr3 #-}
foldr3 :: (a -> b -> c -> d -> d) -> Stream a -> Stream b -> Stream c -> d
foldr3 k = go
  where
    go (a:-as) (b:-bs) (c:-cs) = k a b c (go as bs cs)


foldr3_left :: (a -> b -> c -> d -> e) -> a ->
               (Stream b -> Stream c -> d) -> Stream b -> Stream c -> e
foldr3_left k a r (b:-bs) (c:-cs) = k a b c (r bs cs)

-- foldr3 k n xs ys zs = foldr (foldr3_left k n) (\_ _ -> n) xs ys zs
{-# RULES
"foldr3/left"   forall k (g::forall b.(a->b->b)->b).
                foldr3 k (build g) = g (foldr3_left k)
 #-}

------

{-# NOINLINE [1] zip #-}
zip :: Stream a -> Stream b -> Stream (a, b)
zip (a:-as) (b:-bs) = (a,b) :- zip as bs

{-# INLINE [0] zipFB #-}
zipFB :: ((a, b) -> c -> d) -> a -> b -> c -> d
zipFB c = \x y r -> (x,y) `c` r

{-# RULES
"zip"      [~1] forall xs ys. zip xs ys = build (\c -> foldr2 (zipFB c) xs ys)
"zipList"  [1]  foldr2 (zipFB (:-)) = zip
 #-}

----------------------------------------------

{-# NOINLINE [1] zip3 #-}
zip3 :: Stream a -> Stream b -> Stream c -> Stream (a, b, c)
zip3 (a:-as) (b:-bs) (c:-cs) = (a,b,c) :- zip3 as bs cs

{-# INLINE [0] zip3FB #-}
zip3FB :: ((a,b,c) -> xs -> xs') -> a -> b -> c -> xs -> xs'
zip3FB cons = \a b c r -> (a,b,c) `cons` r

{-# RULES
"zip3"       [~1] forall as bs cs. zip3 as bs cs = build (\c -> foldr3 (zip3FB c) as bs cs)
"zip3List"   [1]          foldr3 (zip3FB (:-)) = zip3
 #-}

----------------------------------------------

-- 'zipWith' is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# NOINLINE [1] zipWith #-}
zipWith :: (t -> t1 -> a) -> Stream t -> Stream t1 -> Stream a
zipWith f = go
  where
    go (x:-xs) (y:-ys) = f x y :- go xs ys


{-# INLINE [0] zipWithFB #-}
zipWithFB :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
zipWithFB c f = \x y r -> (x `f` y) `c` r

{-# RULES       -- See Note [Fusion for zipN/zipWithN]
"zipWith"       [~1] forall f xs ys.    zipWith f xs ys = build (\c -> foldr2 (zipWithFB c f) xs ys)
"zipWithList"   [1]  forall f.  foldr2 (zipWithFB (:-) f) = zipWith f
  #-}


{-# NOINLINE [1] zipWith3 #-}
zipWith3 :: (t -> t1 -> t2 -> a) -> Stream t -> Stream t1 -> Stream t2 -> Stream a
zipWith3 z = go
  where
    go (a:-as) (b:-bs) (c:-cs) = z a b c :- go as bs cs


{-# INLINE [0] zipWith3FB #-}
zipWith3FB :: (d -> xs -> xs') -> (a -> b -> c -> d) -> a -> b -> c -> xs -> xs'
zipWith3FB cons func = \a b c r -> func a b c `cons` r

{-# RULES
"zipWith3"      [~1] forall f as bs cs.   zipWith3 f as bs cs = build (\c -> foldr3 (zipWith3FB c f) as bs cs)
"zipWith3List"  [1]  forall f.   foldr3 (zipWith3FB (:-) f) = zipWith3 f
 #-}


-- Inline so that fusion `foldr` has an opportunity to fire.
-- See Note [Inline @unzipN@ functions] in GHC/OldList.hs.
{-# INLINE unzip #-}
unzip :: Stream (a, a1) -> (Stream a, Stream a1)
unzip = foldrS (\(a,b) ~(as,bs) -> (a:-as,b:-bs))


-- Inline so that fusion `foldr` has an opportunity to fire.
-- See Note [Inline @unzipN@ functions] in GHC/OldList.hs.
{-# INLINE unzip3 #-}
unzip3 :: Stream (a, a1, a2) -> (Stream a, Stream a1, Stream a2)
unzip3 = foldrS (\(a,b,c) ~(as,bs,cs) -> (a:-as,b:-bs,c:-cs))


{-# INLINE unzipWith #-}
unzipWith :: (t -> (a, a1)) -> Stream t -> (Stream a, Stream a1)
unzipWith f = foldrS (\(f->(a,b)) ~(as,bs) -> (a:-as,b:-bs))

-----------

instance Z.Semialign Stream where
  {-# INLINE align #-}
  align = Data.Stream.Internal.zipWith These

instance Z.Zip Stream where
  {-# INLINE zip #-}
  zip = Data.Stream.Internal.zip

  {-# INLINE zipWith #-}
  zipWith = Data.Stream.Internal.zipWith

instance Z.Repeat Stream where
  {-# INLINE repeat #-}
  repeat = Data.Stream.Internal.repeat

instance Z.Unzip Stream where
  {-# INLINE unzip #-}
  unzip = Data.Stream.Internal.unzip

  {-# INLINE unzipWith #-}
  unzipWith = Data.Stream.Internal.unzipWith

  -- unzipWith f ((f->(a,b)):-xs)
  --   | ~(as, bs) <- unzipWith f xs
  --   = (a:-as, b:-bs)

  -- unzip ((a,b):-rst)
  --   | ~(as, bs) <- unzip rst
  --   = (a:-as, b:-bs)

-- instance W.Filterable Stream where
--   {-# INLINE filter #-}
--   filter = Data.Stream.Internal.filter
--
--   {-# INLINE catMaybes #-}
--   catMaybes = Data.Stream.Internal.catMaybes
--
--   {-# INLINE mapMaybe #-}
--   mapMaybe  = Data.Stream.Internal.mapMaybe

  -- filter p ~(a :- as)
  --   | p a = a :- filter p as
  --   | let = filter p as

------------

{-# INLINE unfoldrS #-}
unfoldrS f b0 =
  build \c ->
    let go b = case f b of (a, new_b) -> a `c` go new_b
     in go b0

