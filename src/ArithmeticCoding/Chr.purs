-- | This module contains a wrapper for arbitrary character types, namely 'Chr'.
-- | This data type extends given alphabet (i.e. character type) with a special
-- | value 'End', that indicates end of string.
-- |
-- | @data Chr a = Chr a | End@
-- | The only semantical difference between 'Chr' and 'Maybe' is that
-- | @compare (Chr a) End = LT@ while @compare (Just a) Nothing = LT@.
module ArithmeticCoding.Chr
       ( Chr (..)
       , isEnd
       , wrap
       , unwrap
       )
where

import Control.Applicative (class Applicative, pure)
import Data.Eq (class Eq)
import Data.Foldable (class Foldable, foldMap)
import Data.Functor (map)
import Data.Monoid (class Monoid, mempty)
import Data.Ord (class Ord, compare)
import Data.Ordering (Ordering(..))
import Data.Semigroup (class Semigroup, (<>))
import Data.Show (class Show, show)

data Chr a = Chr a | End

derive instance eqChr :: Eq a => Eq (Chr a)

instance showChr :: Show a => Show (Chr a) where
  show End = "End"
  show (Chr a) = "Chr " <> show a

instance ordChr :: Ord a => Ord (Chr a) where
  compare (Chr a) (Chr b) = compare a b
  compare (Chr a) End     = LT
  compare End     (Chr a) = GT
  compare End     End     = EQ


isEnd :: forall a. Chr a -> Boolean
isEnd End = true
isEnd _   = false


-- | Add the End character to the end.
-- |
-- | E.g. @wrap [1,2,3] = [Chr 1, Chr 2, Chr 3, End]@
wrap :: forall f a. Applicative f => Semigroup (f (Chr a)) =>
             f a -> f (Chr a)
wrap f = map Chr f <> pure End


unwrap :: forall f a. Foldable f => Monoid (f a) => Applicative f =>
          f (Chr a) -> f a
unwrap = foldMap go
  where
    go End = mempty
    go (Chr a) = pure a
