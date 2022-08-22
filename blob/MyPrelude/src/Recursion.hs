{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Recursion
  ( module Recursion
  , module Data.Functor.Foldable
  , module Data.Fix
  )
  where

import Base
import Data.Functor.Foldable
import Data.Fix (Fix (..), Nu (..), Mu (..))


class    (Recursive f, Corecursive f) => Birecursive f
instance (Recursive f, Corecursive f) => Birecursive f

pattern Rec :: Birecursive t => Base t t -> t
pattern Rec a <- (project->a)
  where Rec a = embed a

