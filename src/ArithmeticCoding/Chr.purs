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
       )
where

import Data.Eq (class Eq)
import Data.Ord (class Ord, compare)
import Data.Ordering (Ordering (..))
import Data.Show (class Show, show)
import Data.Semigroup ((<>))

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
