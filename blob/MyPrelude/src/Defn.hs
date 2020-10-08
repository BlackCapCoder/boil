{-# LANGUAGE FlexibleContexts #-}
module Defn where

import Base


-- Defn is a nullary datatype that is convenient
-- for type level programming.
data Defn = Defn
  deriving
    ( Show
    )

type IsDefn
   = Coercible Defn


defn :: IsDefn a => a
defn = coerce Defn


-- Usecase:
--
-- newtype Decidible = Decidible Defn
--
-- mkDecidible = defn :: Decidible
--
--
-- ie: 'defn' gives us a common constructor
-- for any datatype that is defined in terms of defn

----

newtype NT a = NT a


nt :: (Coercible a (NT a), Coercible (NT a) (f a)) => a -> f a
nt = coerce

