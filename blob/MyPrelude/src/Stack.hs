module Stack where

import Base
import Data.Traversable
import qualified Data.List as L
import qualified Data.Sequence as S
import GHC.Exts (IsList (..))
import Data.Foldable as F


-- TODO: Totality type var to avoid duplicate methods?
class Stack t
  where

  peek :: t a -> Maybe a        -- safeHead
  tail :: t a -> Maybe (t a)    -- safeTail
  pop  :: t a -> Maybe (a, t a) -- peek + tail, uncons
  push :: a -> t a -> t a       -- cons

  peek = fmap fst . pop
  tail = fmap snd . pop
  pop  = liftA2 (,) . peek <*> tail

  -- unsafe variants
  peek' :: t a -> a        -- head, copoint, extract
  tail' :: t a -> t a      -- tail
  pop'  :: t a -> (a, t a)

  peek' = fst . pop'
  tail' = snd . pop'
  pop'  = liftA2 (,) peek' tail'

  -- feature creep
  nil   :: t a         -- mempty
  isNil :: t a -> Bool -- null

  isNil = isNothing . peek

  default nil :: Monoid (t a) => t a
  nil = mempty

popDefault  = Just . pop'
pop'Default = fromJust . pop


-- Generic pattern syn
--
infixr 5 :-
pattern (:-) x xs <- (pop -> Just (x, xs))
  where (:-) = push

pattern Nil <- (isNil->True)
  where Nil = nil


-- Deriving
--
newtype WrappedStack  t a
      = WrappedStack (t a)
  deriving newtype
    ( Stack
    )
  deriving stock
    ( Show
    )


fromListDefault
  = foldr push nil

apDefault = go where
  go (f :- fs) (x :- xs)
    = f x :- apDefault fs xs
  go _ _
    = nil

traverseDefault f =
  \case
     x :- xs -> liftA2 (:-) (f x) (traverse f xs)
     _       -> pure nil
  -- nil     -> unsafeCoerce nil

foldrDefault f a =
  \case x :- xs -> f x (foldr f a xs)
        _       -> a

duplicateDefault = \case
  s@(_ :- xs) -> s :- duplicate xs
  _           -> nil -- Either this is unreachable, or extract is unsafe


instance Stack t => Copointed (WrappedStack t) where
  copoint = peek'

instance Stack t => Pointed (WrappedStack t) where
  point = push `flip` nil

instance Stack t => Semigroup (WrappedStack t a) where
  l <> r = foldr push r l

instance Stack t => Monoid (WrappedStack t a) where
  mempty = nil

instance Stack t => Foldable (WrappedStack t) where
  foldr = foldrDefault

instance Stack t => Traversable (WrappedStack t) where
  traverse = traverseDefault

instance Stack t => Functor (WrappedStack t) where
  fmap = fmapDefault

instance Stack t => Comonad (WrappedStack t) where
  extract   = copoint
  duplicate = duplicateDefault

instance Stack t => Applicative (WrappedStack t) where
  pure = point

  -- Zippy version
  (<*>) = apDefault

  -- (<*>)  = ap
  -- liftA2 = liftM2

instance Stack t => Monad (WrappedStack t) where
  (>>=) = flip foldMap

instance Stack t => IsList (WrappedStack t a) where
  type Item (WrappedStack t a) = a
  fromList = fromListDefault
  toList   = F.toList

----

instance Stack []
  where

  peek  = safeHead
  pop   = L.uncons
  peek' = head
  tail' = L.tail
  push  = (:)
  nil   = []
  isNil = null

instance Stack S.Seq
  where

  peek' = head
  peek  = safeHead
  tail' = S.drop 1
  tail  = tail' <? null
  nil   = S.empty
  isNil = S.null
  push  = (S.<|)

