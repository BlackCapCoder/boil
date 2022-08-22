module Data.Stream.Type where

import Prelude (Show (..))

data Stream a = !a :- Stream a
  deriving
    ( Show
    )

infixr 5 :-

