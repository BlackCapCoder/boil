{-# LANGUAGE UndecidableSuperClasses #-}
module Utils where

import Base
import Data.Align
import Data.These


-- Constraint composition
infixr 6 <>
class    (a, b) => a <> b
instance (a, b) => a <> b

-- Fold with monoid unit
mfoldl  f = foldl  f mempty
mfoldl' f = foldl' f mempty
mfoldr  f = foldr  f mempty
mfoldr' f = foldr' f mempty


-- a1 a2 a3
-- b1 b2 b3
-- c1 c2 c3
--
-- a1
-- a2 b1
-- a3 b2 c1
--    b3 c2
--       c3
--
-- [a1], [a2, b1], [a3,b2,c1], [b3, c2], [c3]

-- Diagonalization ensures that we don't get "stuck" exploring
-- an infinite structure.
-- Consider the example above: if there were an infinite number
-- of a's, in-order traversal would never consider b or c.
--
diagonal :: Foldable t => t [a] -> [[a]]
diagonal
  = diagonalEx (:)


-- Generic version of diagonal
diagonalEx
  :: (Foldable t, Semialign f, Pointed f, Monoid (f a), Monoid (f (f a)))
  => (∀ a. a -> f a -> f a) -> t (f a) -> f (f a)
diagonalEx cons
  = mfoldr \x -> alignWith (these point id cons) x . cons mempty


