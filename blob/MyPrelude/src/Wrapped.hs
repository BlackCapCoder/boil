module Wrapped where

import Base
import Coerce (Parametrically)

import Data.Functor.Bind as B
import Data.These (These (..))


newtype WrappedPointed f a = WrappedPointed (f a)
  deriving newtype
    ( Pointed, Functor, Apply, Semigroup
    , Semialign, Zip
    )


instance (Pointed f, Apply f) => Applicative (WrappedPointed f) where
  pure   = point
  liftA2 = liftF2
  (<*>)  = (<.>)
  ( *>)  = (.>)
  (<* )  = (<.)

instance
  ( Pointed f, Bind f
  , Parametrically Coercible f ) => Monad (WrappedPointed f) where
  (>>=) = (>>-)

instance
   ( Bind f
   , Parametrically Coercible f
   ) => Bind (WrappedPointed f)
 where
   (>>-) :: ∀ a b. WrappedPointed f a -> (a -> WrappedPointed f b) -> WrappedPointed f b
   join  :: ∀ a. WrappedPointed f (WrappedPointed f a) -> WrappedPointed f a

   (>>-) = coerce do (>>-)  :: f a -> (a -> f b) -> f b
   join  = coerce do B.join :: f (f a) -> f a


-- This is not always correct.
instance (Pointed f, Zip f) => Repeat (WrappedPointed f) where
  repeat = point


------

instance Applicative f => Semialign (WrappedApplicative f) where align   = liftA2 These
instance Applicative f => Zip       (WrappedApplicative f) where zipWith = liftA2
instance Applicative f => Repeat    (WrappedApplicative f) where repeat  = pure

