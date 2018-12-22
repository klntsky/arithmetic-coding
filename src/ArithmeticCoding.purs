module ArithmeticCoding
       ( Focus
       , StopCondition
       , Adaptation
       , StepInfo
       , mkFocus, noAdaptation, increaseWeight, step, encode, encodeWithFocus, average
       , decode, decodeSteps, rebalance, nearestPowerOf2
       , module ArithmeticCoding.Chr
       )
       where

import ArithmeticCoding.Chr (Chr(..), isEnd)
import Data.Big (Big, divide, fromInt)

import Prelude ( class Ord
               , Unit
               , const, identity, map, one, otherwise, when, whenM, zero, bind, discard
               , ($), (&&), (*), (+), (-), (<), (<$>), (<<<), (<=), (>=), (>>=))
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT, evalStateT, execStateT, get, modify_, put)
import Data.Decimal as D
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl, sum)
import Data.Int (ceil, pow)
import Data.List (List(..))
import Data.Map (Map, empty, insert, lookup, submap, toUnfoldable, update, isEmpty)
import Data.Maybe (Maybe(..), fromJust)
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Strong ((***))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), snd, fst)
import Partial.Unsafe (unsafePartial)


-- | Internal state for arithmetic coding.
type Focus char = { lowerBound :: Big
                  , upperBound :: Big
                  , weights :: Map char Int
                  , total :: Int
                  , count :: Int }


-- | Adaptation is a function that changes characters' weights w.r.t. the
-- | character being consumed.
type Adaptation char = Focus char -> char -> Focus char


-- | StepInfo represents a "log entry" that contains all information about one
-- | particular 'decodeStep' computation.
type StepInfo char = { result :: char
                     , focus :: Focus char
                     , oldFocus :: Focus char
                     , total :: Int
                     , shift :: Int
                     , weight :: Int }


-- | A predicate that tells us when to stop decoding (should return true when
-- | called on a "special EOF character", whatever it is).
type StopCondition char = char -> Boolean


-- | Initialize focus data structure using given alphabet of type @f char@.
mkFocus :: forall char f. Ord char => Foldable f =>
           f char -> Focus char
mkFocus f  = foldl insertChar { lowerBound: zero :: Big
                              , upperBound: one :: Big
                              , weights: empty
                              , total: 0
                              , count: 0 } f
  where
    insertChar focus char = focus { weights = insert char 1 focus.weights
                                  , total = focus.total + 1
                                  , count = focus.count + 1 }


-- | Performs one step, consumes one character, narrows the window between
-- | 'lowerBound' and 'upperBound' and rebalances weights w.r.t. given
-- | 'Adaptation'.
--
-- The idea is simple. We know that it is safe to divide Bigs by powers
-- of two, so instead of dividing the interval between lower- and upper- bounds
-- by 'total' (which is the sum of all weights in the Map), we alter the weights a
-- bit, increasing their sum (total') to be 'neareastPowerOf2', and only then
-- we perform division. See 'rebalance'.
step :: forall char. Ord char =>
        Adaptation char -> Focus char -> char -> Focus char
step adapt focus@{ lowerBound, upperBound, weights, total } char
  = let total' = 2 `pow` nearestPowerOf2 total
        weights' = rebalance (total' - total) weights

        -- I suppose that 'submap'ping an ordered Map and then subtracting
        -- the charWeight is faster than filtering all k/v pairs, because only a
        -- subset of Map gets involved.
        charWeight :: Int
        charWeight = unsafePartial $ fromJust $ lookup char weights'

        -- sum of weights of all characters less than char
        ltSum :: Int
        ltSum = sum (submap Nothing (Just char) weights') - charWeight

        -- interval corresponding to a single character with weight = 1.
        piece :: Big
        piece = unsafePartial $ fromJust $ (upperBound - lowerBound) `divide` fromInt total'
        lowerBound' = lowerBound  + piece * fromInt ltSum
        upperBound' = lowerBound' + piece * fromInt charWeight
    in
     adapt (focus { lowerBound = lowerBound'
                  , upperBound = upperBound'
                  }) char


-- | Performs 'step' on 'Focus' while consuming characters from given
-- | 'Foldable', returns the final focus.
-- | To get the actual value, 'average' should be used (It should be noted that
-- | any number from the resulting interval will go).
encode :: forall char f. Ord char => Foldable f =>
          Adaptation char -> f char -> f char -> Focus char
encode adapt = encodeWithFocus adapt <<< mkFocus


-- | Single initial focus can be reused by both 'encodeWithFocus' and 'decode'.
encodeWithFocus :: forall char f. Ord char => Foldable f =>
           Adaptation char -> Focus char -> f char -> Focus char
encodeWithFocus adapt = foldl (step adapt)


-- | Auxiliary function to get the actual encoding result from 'Focus'.
average :: forall t. { lowerBound :: Big
                     , upperBound :: Big | t } -> Big
average { lowerBound, upperBound } =
  unsafePartial $ fromJust $ (lowerBound + upperBound) `divide` fromInt 2


-- | Opposite of 'encode'.
decode :: forall char. Partial => Ord char =>
          Adaptation char -> StopCondition char -> Big -> Focus char ->
          List char
decode adapt condition n = map _.result <<< decodeSteps adapt condition n


-- | Provides additional information about each step of decoding.
decodeSteps :: forall char. Partial => Ord char =>
           Adaptation char -> StopCondition char -> Big -> Focus char ->
           List (StepInfo char)
decodeSteps adapt condition n = go
  where
    go focus =
      case runExcept $ evalStateT (decodeStep adapt focus n) 0 of
        Right _ -> Nil
        Left si -> Cons si $ if condition si.result
                             then Nil
                             else go si.focus


decodeStep :: forall char. Partial => Ord char =>
                 Adaptation char -> Focus char -> Big ->
                 StateT Int (Except (StepInfo char)) Unit
decodeStep adapt focus@{ lowerBound, upperBound, weights, total, count } n =
  let total' = 2 `pow` nearestPowerOf2 total
      weights' = rebalance (total' - total) weights
      piece = fromJust $ (upperBound - lowerBound) `divide` fromInt total'
  in do
    for_ (toUnfoldable weights' :: List (Tuple char Int)) $ \(Tuple char weight) -> do
      shift <- get
      let lowerBound' = fromInt shift  * piece + lowerBound
          upperBound' = fromInt weight * piece + lowerBound'
      when (n >= lowerBound' && n < upperBound') do
        throwError ({ result: char
                    , oldFocus: focus
                    , focus: adapt (focus { lowerBound = lowerBound'
                                          , upperBound = upperBound'
                                          }) char
                    , total: total'
                    , shift: shift
                    , weight: weight })
      put (shift + weight)


-- | For given n :: Int, find the least k :: Int, 2 ^ k >= n
nearestPowerOf2 :: Int -> Int
nearestPowerOf2 = ceil <<< D.toNumber <<< D.log2 <<< D.fromInt


-- | Rebalance the weights so that no "free" intervals are left.
rebalance :: forall char. Ord char =>
             Int -> Map char Int -> Map char Int
rebalance 0 w = w
rebalance d w | isEmpty w = w
              | otherwise = fst $ merge $ runExcept $ execStateT go (Tuple w d)
  where
    -- To exit when d is 0
    isDone = (_ <= 0) <<< snd <$> get

    merge :: forall a. Either a a -> a
    merge = identity ||| identity

    go = forever do
      w' <- fst <$> get
      for_ (toUnfoldable w' :: Array (Tuple char Int)) $
        \(Tuple k v) -> do
          modify_ (insert k (v + 1) *** (_ - 1))
          whenM isDone do
            get >>= throwError


-- | 'Adaptation' that does nothing.
noAdaptation :: forall a. Adaptation a
noAdaptation = const


-- | 'Adaptation' that increases the weight of the given character by one.
increaseWeight :: forall a. Ord a => Adaptation a
increaseWeight focus char =
  focus { weights = update (Just <<< (_ + 1)) char focus.weights
        , total = focus.total + 1 }
