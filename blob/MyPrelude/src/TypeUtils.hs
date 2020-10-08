{-# LANGUAGE PolyKinds #-}
module TypeUtils where


infixr 0 $
type ($) :: ∀ k. k -> k
type ($) a = a

infixr 9 .
type (f . g) a = f (g a)

infixl 4 <*>
type (f <*> g) a = f a (g a)

infixl 1 >>=
infixr 1 =<<

type (=<<) f g a = f (g a) a -- (f . g) a a
type (>>=) g f a = f (g a) a

-- infixl 0 <--
-- type (<--) a b = b -> a

type Id   a     = a
type Pure a b   = a
type Join f a   = f a a -- (f =<< Id) a
type Flip f a b = f b a

-- type Ex f = ∀ b. f b


