{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-inaccessible-code #-}
module Logic.Not where

import Base
import Iso
import Data.Functor.Contravariant
import Unsafe.Coerce


-- newtype Lie
--       = Lie { eatSoap :: ∀ a. a }
--
newtype Not a
      = Not { elimNot :: a -> ∀ b. b }

-- instance Contravariant Not where
--   contramap ab (Not b)
--     = Not (b . ab)

-----

-- contradicts :: a -> Not a -> b
-- contradicts = flip elimNot
-- {-# INLINE [1] contradicts #-}
--
-- noncontra :: Not (a, Not a)
-- noncontra = Not $ uncurry contradicts

-- nor :: Not a -> Not b -> Not (Either a b)
-- nor a b = Not $ either (elimNot a) (elimNot b)

-- notNot :: a -> Not (Not a)
-- notNot a = contramap (a,) noncontra
-- {-# INLINE [1] notNot #-}

-- notMe :: (a -> Not a) -> Not a
-- notMe f = Not $ elimNot =<< f

-- meNot :: (Not a -> a) -> Not (Not a)
-- meNot f = notMe (notNot . f)


-- notVoid :: Not Void
-- notVoid = Not absurd

-- notLie :: Not Lie
-- notLie = Not eatSoap
--
-- voidLie :: Void <-> Lie
-- voidLie = Iso absurd eatSoap
--
-- dirty :: (a -> Lie) -> Not a
-- dirty f = Not (eatSoap . f)
-- {-# INLINE [1] dirty #-}

----

-- Classical, non-constructive logic
class Classical where
  lem :: Either a (Not a) -- Law of excluded middle
  lem = error "lem"

-- Since Classical is nullary there can only
-- ever be a single instance, which is now
-- locked behind a constraint that can never
-- be satisfied.
instance Int ~ Bool => Classical

-- This type withnesses a constraint
data Proof p = p => Proof

-- Discharge a Classical constraint, by saying
-- "I am going to allow a classical argument here."
classically :: (Classical => a) -> a
classically a = case lie of Proof -> a
  where
    truth = Proof              :: Proof (Int ~ Int)
    lie   = unsafeCoerce truth :: Proof (Int ~ Bool)
{-# NOINLINE classically #-}
{-# RULES "classically" ∀ a. classically a = a #-}

-- Double negation
-- dnot :: Classical => Not (Not a) -> a
-- dnot (Not f) = either id f lem
-- {-# NOINLINE dnot #-}
-- {-# RULES "dnot/notNot" ∀ a. dnot (notNot a) = a #-}
-- {-# RULES "notNot/dnot" ∀ a. notNot (dnot a) = a #-}

-- Prove 'a' by showing that '¬a' leads
-- to a contradiction
-- contradiction :: Classical => (Not a -> Lie) -> a
-- contradiction f = dnot (dirty f)
-- {-# INLINE [1] contradiction #-}
--
-- {-# RULES
--    "dnot.dirty/contradiction"
--      ∀ a. dnot (dirty a) = contradiction a;
--
--    "contradiction/contradicts"
--      ∀ a. contradiction (contradicts a) = a;
--    "contradiction/elimNot"
--      ∀ a. contradiction (\n -> elimNot n a) = a;
--    "contradiction/Not"
--      ∀ a. contradiction (\(Not f) -> f a) = a;
-- #-}

