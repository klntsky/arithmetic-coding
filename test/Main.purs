module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import ArithmeticCoding
import Test.Assert
import Data.Traversable
import Data.Foldable
import Data.Maybe
import Data.Tuple
import Data.Array
import Data.String
import Partial.Unsafe
import Data.Big


main :: Effect Unit
main = do
  withAlphabet
    "abcdefghijklm"
    [ "abcdef"
    , "abcd"
    , "aaaaa"
    , "mmmmm"
    , "amamamamamamam"
    ]

  withAlphabet
    ""
    [ "" ]

  withAlphabet
    "a"
    [ "a"
    , "aa"
    , "aaa"
    , "aaaa"
    , "aaaaaaaaa"
    ]

  let longStr = "Side-effects, mutation & other escape hatches. These aren't usually the shiny selling points of a language; but being able to bridge toward a part of a codebase without an elaborate interop/rewrite is crucial for us at Facebook. OCaml defaults to immutable and functional code, but having the escape hatches makes the initial adoption sometimes simply possible."
  withAlphabet longStr [ longStr ]

  assert $ all (uncurry (\x y -> nearestPowerOf2 x == y))
    [ Tuple 2 1
    , Tuple 3 2
    , Tuple 4 2
    , Tuple 5 3
    , Tuple 1024 10
    , Tuple 1025 11
    ]


withAlphabet :: String -> Array String -> Effect Unit
withAlphabet alphabet' inputs = do
  let alphabet = normalize $ nub $ toCodePointArray alphabet'
      focus = mkFocus alphabet
  for_ [noAdaptation, increaseWeight] $ \adapt -> do
    for_ inputs $ \input' -> do
      let input = normalize $ toCodePointArray input'
          result :: Big
          result = average $ encodeWithFocus adapt focus input
          output = fromFoldable $ unsafePartial $ decode adapt isEnd result focus
      assert'
        ("failed for input " <> show input' <> " in alphabet " <> show alphabet')
        (input == output)
