module Data.Foldable.Unfoldable where

import Base


class Unfoldable f where
  unfold :: Foldable g => g a -> f a


instance Unfoldable [] where
  unfold = foldr (:) []

instance Unfoldable Maybe where
  unfold = foldr (const . pure) empty
