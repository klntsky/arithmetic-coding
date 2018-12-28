module Data.Big
       ( Big
       , fromString
       , divide
       , toFixed
       , fromInt
       )
       where

import Prelude (class CommutativeRing, class Eq, class Ord, class Ring
               , class Semiring, class Show, Ordering(..), join, map
               , ($))

import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Foreign (Foreign, readNull, readNullOrUndefined, unsafeFromForeign)


foreign import data Big :: Type
foreign import fromStringI :: String -> Foreign
foreign import fromInt :: Int -> Big
foreign import toString :: Big -> String
foreign import toFixedI :: Fn2 Big Int String
foreign import equals :: Fn2 Big Big Boolean
foreign import compareTo :: Fn2 Big Big Int
foreign import addI :: Fn2 Big Big Big
foreign import zeroI :: Big
foreign import mulI :: Fn2 Big Big Big
foreign import oneI :: Big
foreign import subtract :: Fn2 Big Big Big
foreign import divideI :: Fn2 Big Big Foreign


instance showBig :: Show Big where
  show = toString

instance bigDecimalEq :: Eq Big where
  eq = runFn2 equals

instance bigDecimalOrd :: Ord Big where
  compare a b = case runFn2 compareTo a b of
    0 -> EQ
    -1 -> LT
    _ -> GT

instance bigDecimalSemiring :: Semiring Big where
  add = runFn2 addI
  zero = zeroI
  mul = runFn2 mulI
  one = oneI

instance bigDecimalRing :: Ring Big where
  sub = runFn2 subtract

instance bigDecimalCommutativeRing :: CommutativeRing Big


fromString :: String -> Maybe Big
fromString str =
  case runExcept (readNullOrUndefined (fromStringI str)) of
    Left _ -> Nothing
    Right x -> map unsafeFromForeign x


toFixed :: Big -> Int -> String
toFixed = runFn2 toFixedI


divide :: Big -> Big -> Maybe Big
divide a b = map unsafeFromForeign $ join $ hush $ runExcept $ readNull $ runFn2 divideI a b
