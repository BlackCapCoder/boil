{-# LANGUAGE DerivingVia #-}
module Wrapped
  ( WrappedPoint       (..)
  , WrappedUnit        (..)
  , WrappedApplicative (..)
  , WrappedMonoid      (..)
  , WrappedCategory    (..)
  , WrappedApply       (..)
  )
  where

import Base
import Unit
import Utils (Parametrically)

import Data.Functor.Bind as B
import Data.Functor.Extend
import Data.These (These (..))
import Data.Semigroup (WrappedMonoid (..))
import Data.Semigroupoid -- (WrappedCategory (..))


newtype WrappedUnit u = WrappedUnit u
  deriving newtype
    ( Unit, Semigroup
    )

newtype WrappedApply f a
      = WrappedApply (f a)
  deriving newtype
    ( Functor, Apply, Pointed
    )
  deriving
    ( Applicative, Semialign, Zip
    )
  via WrappedPoint f

instance (Unit u, Semigroup u) => Monoid (WrappedUnit u) where
  mempty = unit


-------

newtype WrappedPoint f a = WrappedPoint (f a)
  deriving newtype
    ( Pointed, Copointed
    , Functor, Apply, Semigroup
    , Semialign, Zip
    )

instance (Pointed f, Apply f) => Applicative (WrappedPoint f) where
  pure   = point
  liftA2 = liftF2
  (<*>)  = (<.>)
  ( *>)  = (.>)
  (<* )  = (<.)

instance
  ( Pointed f, Bind f
  , Parametrically Coercible f ) => Monad (WrappedPoint f) where
  (>>=) = (>>-)

instance
  ( Copointed f
  , Extend f
  , Parametrically Coercible f
  ) => Comonad (WrappedPoint f)
  where
    extract   = copoint
    duplicate = duplicated
    extend    = extended

-- This is conceptually only correct for infinite structures
instance (Pointed f, Zip f) => Repeat (WrappedPoint f) where
  repeat = point


instance
   ( Bind f
   , Parametrically Coercible f
   ) => Bind (WrappedPoint f)
 where
   (>>-) :: ∀ a b. WrappedPoint f a -> (a -> WrappedPoint f b) -> WrappedPoint f b
   join  :: ∀ a. WrappedPoint f (WrappedPoint f a) -> WrappedPoint f a

   (>>-) = coerce do (>>-)  :: f a -> (a -> f b) -> f b
   join  = coerce do B.join :: f (f a) -> f a

instance
  ( Extend f
  , Parametrically Coercible f
  ) => Extend (WrappedPoint f)
  where
    duplicated :: ∀ a. WrappedPoint f a -> WrappedPoint f (WrappedPoint f a)
    extended   :: ∀ a b. (WrappedPoint f a -> b) -> WrappedPoint f a -> WrappedPoint f b

    duplicated = coerce do duplicated :: f a -> f (f a)
    extended   = coerce do extended   :: (f a -> b) -> f a -> f b

------

instance Applicative f => Semialign (WrappedApplicative f) where align   = liftA2 These
instance Applicative f => Zip       (WrappedApplicative f) where zipWith = liftA2
instance Applicative f => Repeat    (WrappedApplicative f) where repeat  = pure

------

instance (Pointed f, Unit a) => Unit (WrappedPoint    f a  ) where unit = point unit
instance Monoid m            => Unit (WrappedMonoid   m    ) where unit = mempty
instance Category k          => Unit (WrappedCategory k a a) where unit = id

