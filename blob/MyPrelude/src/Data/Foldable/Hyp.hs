module Data.Foldable.Hyp where

newtype H a b = H { invoke ∷ H b a → b }


