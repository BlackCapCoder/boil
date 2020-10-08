{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Coerce where

import Base


type Parametrically c f
   = ∀ a b. c a b => c (f a) (f b) :: Constraint

class    Parametrically c f => Para c f
instance Parametrically c f => Para c f

infixl 8 ∀.

type (∀.) = Para

----

infix 4 ~=
type (~=) = Coercible

para :: ∀ f x y. (x ~= y, (~=) ∀. f) => f x -> f y
para = coerce


class a ~= b => Newtype a b | a -> b
  where
    pack   :: b -> a
    unpack :: a -> b

    pack   = coerce ; {-# INLINE pack   #-}
    unpack = coerce ; {-# INLINE unpack #-}

instance ((~=) ∀. f, f a ~= a) => Newtype (f a) a


-- ala Sum foldMap [1,2,3,4]
ala :: (Newtype n o, Newtype n' o')
    => (o -> n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof
  = ala' pa hof id

ala' :: (Newtype n o, Newtype n' o')
     => (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
ala' _ hof f
  = unpack . hof (pack . f)

under :: (Newtype n o, Newtype n' o') => (o -> n) -> (n -> n') -> (o -> o')
under _
  = coerce -- \f -> unpack . f . pack

over :: (Newtype n o, Newtype n' o') => (o -> n) -> (o -> o') -> (n -> n')
over _
  = coerce -- \f -> pack . f . unpack

-- foldMap `via` Sum
via
  = flip ala

