{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Base
  (
    -- * Standard types, classes and related functions

    -- ** Basic data types
    Bool(False, True),
    (&&), (||), not, otherwise,

    Ordering(LT, EQ, GT),
    Char, String,

    -- *** Tuples
    fst, snd, curry, uncurry,

    -- ** Basic type classes
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),

    -- ** Numbers

    -- *** Numeric types
    Int, Integer, Float, Double,
    Rational, Word,

    -- *** Numeric type classes
    Num(abs, signum),
    (+), (-), (*), fromInteger, fromNatural,
    plus, minus, times,
    zero, one, isZero, isOne,

    Integral(div, mod, divMod, toInteger),
    Euclidean (quotRem, quot, rem, degree),
    GcdDomain (divide, gcd, lcm, coprime),

    Real(toRational),
    Fractional(),
    Field (),
    fromRational, recip, (/),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    -- *** Numeric functions
    subtract,
    even, odd, (^), (^^),
    S.fromIntegral,
    realToFrac,
    gcdExt,

    -- ** Semigroups and Monoids
    Semigroup((<>)),
    Monoid(mempty, mappend, mconcat),

    -- ** Monads and functors
    Functor(fmap, (<$)),

    -- ** Folds and traversals
    Foldable
      (-- elem,   -- :: (Foldable t, Eq a) => a -> t a -> Bool
      fold,   -- :: Monoid m => t m -> m
      foldMap,   -- :: Monoid m => (a -> m) -> t a -> m
      foldr,     -- :: (a -> b -> b) -> b -> t a -> b
      foldr', -- :: (a -> b -> b) -> b -> t a -> b
      foldl,     -- :: (b -> a -> b) -> b -> t a -> b
      foldl', -- :: (b -> a -> b) -> b -> t a -> b
      foldr1,    -- :: (a -> a -> a) -> t a -> a
      foldl1,    -- :: (a -> a -> a) -> t a -> a
      maximum,   -- :: (Foldable t, Ord a) => t a -> a
      minimum,   -- :: (Foldable t, Ord a) => t a -> a
      -- product,   -- :: (Foldable t, Num a) => t a -> a
      -- sum,       -- :: Num a => t a -> a
      toList -- :: Foldable t => t a -> [a]
      ),

    S.sum, S.product, sum', product',
    foldMapP, foldMapT,
    asum,

    Traversable (traverse, sequenceA, mapM, sequence),

    -- ** Miscellaneous functions
    const, flip, ($), until,
    asTypeOf, error, errorWithoutStackTrace, undefined,
    seq, ($!),

    -- * List operations
    map, (++), filter,
    head, last,
    -- tail,
    init, null, length, (!!),
    reverse,
    -- *** Special folds
    and, or, any, all,
    concat, concatMap,
    -- ** Building lists
    -- *** Scans
    scanl, scanl1, scanr, scanr1,
    -- *** Infinite lists
    iterate, replicate, cycle,
    -- ** Sublists
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    -- ** Searching lists
    -- notElem,
    lookup,
    -- ** Zipping and unzipping lists
    zip3, zipWith3, unzip3,
    -- ** Functions on strings
    lines, words, unlines, unwords,
    safeHead, safeLast,

    -- * Converting to and from @String@
    -- ** Converting to @String@
    ShowS,
    Show(showsPrec, showList, show),
    shows,
    showChar, showString, showParen,
    -- ** Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads, readParen, read, lex,

    -- * Basic Input and output
    IO,
    -- ** Simple I\/O operations
    -- All I/O functions defined here are character oriented.  The
    -- treatment of the newline character will vary on different systems.
    -- For example, two characters of input, return and linefeed, may
    -- read as a single newline character.  These functions cannot be
    -- used portably for binary I/O.
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    -- ** Exception handling in the I\/O monad
    IOError, ioError, userError


    , unfoldr

    , module Data.Kind
    , module GHC.TypeNats
    , module Data.Coerce
    , module Data.Maybe
    , module Data.Either
    , module Control.Category
    , module Control.Monad
    , module Control.Applicative
    , module Data.Proxy
    , module Data.Void
    , module Data.Function

    , module Data.Pointed
    , module Data.Copointed
    , module Control.Comonad

    , module Data.Zip -- zip, unzip, zipWith, repeat
    , module Data.These

    , module Data.Functor.Apply
    , module Data.Functor.Bind -- join
    , module Data.Functor.Extend

    -- , module Data.Functor.Utils
    , (#.), (.#), ( #$), ($#)
    , pamf, (<&>)

    ------------------

    , fi, comparing, swap
    , Semiring, Ring (..)

    -- , Sum (..), Product (..)
    , Alt (..), ZipList (..), Down (..)
    , Dual (..)
    -- , Ring (..)

    , (<?), (?>), (<?!), (!?>)

    , Identity (..)

  )
  where


import Data.Function (fix, on, (&))
import Data.Proxy (Proxy (..))
import Data.Void
import Data.Coerce
import Data.Kind
import GHC.TypeNats (Nat (..), KnownNat (..), natVal, natVal', type (+), type (*))
import Control.Category
import Control.Monad hiding (join)
import Control.Applicative

import System.IO
import System.IO.Error
import Data.List hiding (map, head, last, lookup, tail, zip, repeat, zipWith, unzip)
import Data.Either
import Data.Foldable    ( Foldable(..), asum )
import Data.Functor     ( (<$>) )
import Data.Maybe
import Data.Traversable ( Traversable(..) )
import Data.Tuple
import Data.Monoid
import Data.Ord (Down (..), comparing)

import GHC.Base hiding ( foldr, mapM, sequence, map, id, (.), join )
import GHC.Show
import Text.Read

import GHC.Enum
import GHC.Num (Num (abs, signum), subtract)
import GHC.Num (Integer)
import qualified GHC.Num as Num
import GHC.Real hiding ((^), gcd, lcm, fromRational, recip, (/))
import GHC.Float
import Data.Semiring as S
import Data.Euclidean
import Data.Field
import Data.Functor.Identity
-- import Data.Functor.Utils ((#.))

import Control.Comonad
import Data.Pointed
import Data.Copointed

import Data.Zip
import Data.These

import Data.Functor.Apply
import Data.Functor.Bind
import Data.Functor.Extend


{-# INLINE (#.) #-}
infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ab = coerce

{-# INLINE (.#) #-}
infixr 9 .#
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> (a -> c)
(.#) bc _ab = coerce bc

{-# INLINE ( #$) #-}
infixr 0 #$
( #$) :: Coercible a b => (a -> b) -> a -> b
( #$) _ab = coerce

{-# INLINE ($#) #-}
infixr 0 $#
($#) :: Coercible a b => (b -> c) -> a -> c
($#) = coerce


----

fi :: (Integral a, Ring b) => a -> b
fi = S.fromIntegral

{-# INLINE map #-}
map :: Functor f => (a -> b) -> f a -> f b
map = fmap

{-# INLINE pamf #-}
pamf :: Functor f => f a -> (a -> b) -> f b
pamf = flip fmap

{-# INLINE (<&>) #-}
infixl 5 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = pamf

----

infixr 1 ?>, !?>, <?, <?!

f  ?> g = liftA2 (<$) g $ guard . f
f !?> g = not.f ?> g

f <?  g = g  ?> f
f <?! g = g !?> f

{-# SPECIALIZE (?>) :: (a -> Bool) -> (a -> b) -> a -> Maybe b #-}

----

{-# INLINE head #-}
{-# INLINE last #-}
{-# INLINE safeHead #-}
{-# INLINE safeLast #-}

head,     last     :: ∀ t a.   _ => t a -> a
safeHead, safeLast :: ∀ t a m. _ => t a -> m a

head = foldr1 \x _ -> x
last = foldr1 \_ x -> x

safeHead = null !?> head
safeLast = null !?> last

-- safeHead
--   = coerce @(t a -> First a)
--   $ foldMap pure

lookup :: ∀ t a b. _ => a -> t (a, b) -> Maybe b
lookup x
  = coerce @(t (a, b) -> First b) $ foldMap
  . coerce @(  (a, b) -> Maybe b)
  $ (==) x . fst ?> snd


-----

instance Semiring s => Semiring (Sum s) where
  zero  = Sum zero
  one   = Sum one
  plus  = coerce (plus  :: s -> s -> s)
  times = coerce (times :: s -> s -> s)

-- This is technically wrong
instance Ring Num.Natural where
  negate = Num.negate

