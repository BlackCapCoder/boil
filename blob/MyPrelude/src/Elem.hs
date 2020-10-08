{-# LANGUAGE UndecidableSuperClasses #-}
module Elem where

import GHC.Base

import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import qualified Data.Set as S
import qualified Data.IntSet as IntSet


class Elem t a | t -> a
  where

  elem, notElem :: a -> t -> Bool

  elem a t
    = not (notElem a t)

  notElem a t
    = not (elem a t)

  {-# MINIMAL elem | notElem #-}


----

infixr 5 ∈, ∉, ∋, ∌

(∈), (∉) :: Elem f a => a -> f -> Bool
(∋), (∌) :: Elem f a => f -> a -> Bool

(∈) = elem
(∉) = notElem
(∋) = flip elem
(∌) = flip notElem

{-# INLINE (∈) #-}
{-# INLINE (∉) #-}
{-# INLINE (∋) #-}
{-# INLINE (∌) #-}

----

newtype ElemFoldable  t a
      = ElemFoldable (t a)
  deriving newtype
    ( Foldable
    )

instance (Foldable f, Eq a) => Elem (ElemFoldable f a) a where
  elem = F.elem

----

instance Ord a => Elem (S.Set a) a
  where
    elem    = S.member
    notElem = S.notMember

instance Elem IntSet.IntSet IntSet.Key
  where
    elem    = IntSet.member
    notElem = IntSet.notMember


