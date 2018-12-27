module UI where

import ArithmeticCoding
import ArithmeticCoding.Chr

import Prelude ( type (~>), Unit, bind, const, discard, flip, map, pure, show
               , when, zero, ($), (*), (/), (<<<), (<>), (-), (==))
import Data.Big (Big, toExact, fromString)
import CSS (backgroundColor, color, grey, marginLeft, px, rgba, width)
import Data.Array as A
import Data.Int (toNumber)
import Data.List  (List(..), all, elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (CodePoint, toCodePointArray)
import Data.String.CodePoints as CP
import Halogen as H
import Halogen.Component as HC
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)


type State = { input :: String
             , alphabet :: String
             , steps :: List (StepInfo (Chr CodePoint))
             , result :: Maybe Big
             , success :: Boolean
             , auto :: Boolean
             , adaptive :: Boolean
             , initialized :: Boolean }


data Query a
  = UpdateInputText String a
  | UpdateAlphabet String a
  | ProcessInput a
  | ToggleAuto Boolean a
  | ToggleAdaptive Boolean a


type Message = Unit

ui :: forall m. H.Component HH.HTML Query Unit Message m
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { input: "we conjure the spirits of the computer with our spells"
                 , alphabet: " abcdefghijklmnopqrstuvwxyz"
                 , steps: Nil
                 , result: Just zero
                 , success: true
                 , auto: false
                 , adaptive: false
                 , initialized: false }

  render :: State -> H.ComponentHTML Query
  render { input, alphabet, result, success, steps, initialized } =
    HH.div_ $
    [ HH.text "Alphabet:"
    , HH.br_
    , HH.input
      [ HP.type_ HP.InputText
      , HP.value alphabet
      , HP.classes [ HH.ClassName "input" ]
      , HP.id_ "alphabet"
      , HE.onValueInput $ HE.input UpdateAlphabet
      ]
    , HH.br_

    , HH.text "Input:"
    , HH.br_
    , HH.textarea
      [ HP.value input
      , HP.classes [ HH.ClassName "input" ]
      , HP.id_ "input"
      , HE.onValueInput $ HE.input UpdateInputText
      ]
    , HH.br_

    , HH.button
      [ HP.title "Calculate result value and render output"
      , HE.onClick $ HE.input_ ProcessInput
      ]
      [ HH.text "Encode & decode" ]

      -- [ ] Auto
    , HH.span
      [ HP.title "Re-render output while typing (use with care)" ]
      [ HH.input [ HP.type_ HP.InputCheckbox
                 , HP.checked false
                 , HE.onChecked $ HE.input ToggleAuto
                 , HP.id_ "auto" ]
      , HH.label [ HP.for "auto" ] [ HH.text "Auto" ]
      ]

      -- [ ] Adaptive
    , HH.span
      [ HP.title "Rebalance weights to promote characters that appear more frequently than the others"  ]
      [ HH.input [ HP.type_ HP.InputCheckbox
                 , HP.checked false
                 , HE.onChecked $ HE.input ToggleAdaptive
                 , HP.id_ "adaptive" ]
      , HH.label [ HP.for "adaptive" ] [ HH.text "Adaptive" ]
      ]

    ] <> if initialized then [
      HH.div_ $ case result of
         Just result' ->
           [ HH.span [ HP.class_ $ HH.ClassName "result" ]
             [ HH.text $ "Result "
             , HH.span
               [ HP.class_ $ HH.ClassName $
                 if success
                 then "status-success"
                 else "status-fail"
               , HP.title $ "This box indicates whether the input is equal to the decoded output" ]
               [ HH.text $
                 if success
                 then "[OK]: "
                 else "[FAIL] (please report as bug): "]
             , HH.text $ toExact result' ]
           , HH.div [ HP.id_ "container" ] <<< A.fromFoldable $
             map (HH.div_ <<< renderStep) steps
           ]
         Nothing ->
           [ HH.br_, HH.text "Invalid input: some characters are not in alphabet!" ]
      ] else []


  mkBounds { lowerBound, upperBound } =
    HH.div [ HP.id_ "bounds-container" ]
    [ HH.div [ HP.classes [ HH.ClassName "one-line", HH.ClassName "lower-bound" ] ]
      [ HH.text $ toExact lowerBound ]

    , HH.div [ HP.classes [ HH.ClassName "one-line", HH.ClassName "upper-bound" ] ]
      [ HH.text $ toExact upperBound ]
    ]

  renderStep { result, total, shift, weight, oldFocus, focus } =
    let w = 1000
        interval = focus.upperBound - focus.lowerBound
    in
    [ mkBounds oldFocus
    , HH.div
      [ CSS.style do
           width (px (toNumber w))
      , HP.classes [ HH.ClassName "progress" ] ]
      [ HH.div
        [ CSS.style do
             width      (px (toNumber $ w * weight / total))
             marginLeft (px (toNumber $ w * shift / total))
             when (isEnd result) do
               color grey
               backgroundColor (rgba 60 60 60 1.0)
        , HP.classes [ HH.ClassName "progress"
                     , HH.ClassName "progress-active"
                     ]
        , HP.title ("shift = " <> show shift <>
                    ", weight = " <> show weight <>
                    ", total = " <> show total <>
                    ", width = " <> show interval)
        ]
        [ HH.text $ case result of
             Chr code -> CP.singleton code
             End -> "ɛ" ] ] ] <>
    if isEnd result
    then [ mkBounds focus
         , HH.div [ HP.id_ "width-container" ]
           [ HH.text $ "width = " <> toExact interval
           ]
         ]
    else []



eval :: forall m. Query ~> HC.ComponentDSL State Query Message m
eval = case _ of
  (UpdateInputText text next) -> do
    state <- H.get
    H.modify_ (_ { input = text })
    when state.auto processInput
    pure next
  (UpdateAlphabet text next) -> do
    state <- H.get
    H.modify_ (_ { alphabet = text })
    when state.auto processInput
    pure next
  (ProcessInput next) -> do
    processInput
    pure next
  (ToggleAuto status next) -> do
    H.modify_ (_ { auto = status })
    when status processInput
    pure next
  (ToggleAdaptive status next) -> do
    H.modify_ (_ { adaptive = status })
    processInput
    pure next
  where
    processInput = do
      H.modify_ (_ { initialized = true })
      { input, alphabet, adaptive } <- H.get
      let alphabet' = wrap $ A.nub $ toCodePointArray alphabet
          input' = wrap $ toCodePointArray input
          isValid = all (flip elem alphabet') input'
          adapt = if adaptive then increaseWeight else noAdaptation
      if isValid then do
        let focus = mkFocus alphabet'
            result :: Big
            result = fromMaybe zero $ fromString $ toExact $
              average $ encodeWithFocus adapt focus input'
            steps = unsafePartial $ decodeSteps adapt isEnd result focus
            success :: Boolean
            success = A.fromFoldable (map _.result steps) == input'
        H.modify_ (_ { result = Just result, steps = steps, success = success })
        else do
        H.modify_ (_ { result = Nothing, steps = Nil })
