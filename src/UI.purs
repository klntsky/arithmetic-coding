module UI where

import ArithmeticCoding

import Prelude ( type (~>), Unit, bind, const, discard, flip, map, pure, show
               , when, zero, ($), (*), (/), (<<<), (<>), (-))
import Data.Big (Big, toFixed)
import CSS (backgroundColor, color, grey, marginLeft, px, rgba, width)
import Data.Array as A
import Data.Int (fromString, toNumber)
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
             , precision :: Int
             , steps :: List (StepInfo (Chr CodePoint))
             , result :: Maybe Big
             , auto :: Boolean
             , adaptive :: Boolean }


data Query a
  = UpdateInputText String a
  | UpdateAlphabet String a
  | UpdatePrecision String a
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
                 , alphabet: "abcdefghijklmnopqrstuvwxyz "
                 , precision: 1000
                 , steps: Nil
                 , result: Just zero
                 , auto: false
                 , adaptive: false }

  render :: State -> H.ComponentHTML Query
  render state =
     HH.div_ $
       [ HH.text "Alphabet:"
       , HH.br_
       , HH.input
         [ HP.type_ HP.InputText
         , HP.value (state.alphabet)
         , HP.classes [ HH.ClassName "input" ]
         , HP.id_ "alphabet"
         , HE.onValueInput (HE.input UpdateAlphabet)
         ]
       , HH.br_

       , HH.text "Input:"
       , HH.br_
       , HH.textarea
         [ HP.value (state.input)
         , HP.classes [ HH.ClassName "input" ]
         , HP.id_ "input"
         , HE.onValueInput (HE.input UpdateInputText)
         ]
       , HH.br_

       , HH.button
         [ HP.title "Calculate result value and render output"
         , HE.onClick (HE.input_ ProcessInput)
         ]
         [ HH.text "Encode & decode" ]

         -- [ ] Auto
       , HH.span
         [ HP.title "Re-render output while typing (use with care)" ]
         [ HH.input [ HP.type_ HP.InputCheckbox
                    , HP.checked false
                    , HE.onChecked (HE.input ToggleAuto)
                    , HP.id_ "auto" ]
         , HH.label [ HP.for "auto" ] [ HH.text "Auto" ]
         ]

         -- [ ] Adaptive
       , HH.span
         [ HP.title "Rebalance weights to promote characters that appear more frequently than the others"  ]
         [ HH.input [ HP.type_ HP.InputCheckbox
                    , HP.checked false
                    , HE.onChecked (HE.input ToggleAdaptive)
                    , HP.id_ "adaptive" ]
         , HH.label [ HP.for "adaptive" ] [ HH.text "Adaptive" ]
         ]


         -- Precision: [   ]
       , HH.span
         [ HP.title "Precision only affects how numbers are printed"  ]
         [ HH.label_ [ HH.text "Precision: " ]
         , HH.input [ HP.type_ HP.InputNumber
                    , HP.value "1000"
                    , HE.onValueInput (HE.input UpdatePrecision) ]
         ]


       , HH.div_ $ case state.result of
           Just result' ->
             [ HH.span [ HP.class_ $ HH.ClassName "result" ]
               [ HH.text $ "Result: " <> toFixed result' state.precision ]
             , HH.div [ HP.id_ "container" ] <<< A.fromFoldable $
               map (HH.div_ <<< renderStep state.precision) state.steps
             ]
           Nothing ->
             [ HH.br_, HH.text "Invalid input: some characters are not in alphabet!" ]
       ]


  mkBounds precision { lowerBound, upperBound } =
    HH.div [ HP.id_ "bounds-container" ]
    [ HH.div [ HP.classes [ HH.ClassName "one-line", HH.ClassName "lower-bound" ] ]
      [ HH.text $ toFixed lowerBound precision ]

    , HH.div [ HP.classes [ HH.ClassName "one-line", HH.ClassName "upper-bound" ] ]
      [ HH.text $ toFixed upperBound precision ]
    ]

  renderStep precision { result, total, shift, weight, oldFocus, focus } =
    let w = 1000 in
    [ mkBounds precision oldFocus
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
                    ", total = " <> show total)
        ]
        [ HH.text $ case result of
             Chr code -> CP.singleton code
             End -> "ɛ" ] ] ] <>
    if isEnd result
    then [ mkBounds precision focus
         , HH.div [ HP.id_ "width-container" ]
           [ HH.text $ "width = " <> toFixed (focus.upperBound - focus.lowerBound) precision
           ]
         ]
    else []



eval :: forall m. Query ~> HC.ComponentDSL State Query Message m
eval = case _ of
  (UpdatePrecision text next) -> do
    state <- H.get
    H.modify_ (_ { precision = fromMaybe 1000 $ fromString text })
    when state.auto processInput
    pure next

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
      state <- H.get
      let alphabet' = normalize $ A.nub $ toCodePointArray state.alphabet
          input' = normalize $ toCodePointArray state.input
          isValid = all (flip elem alphabet') input'
          adapt = if state.adaptive then increaseWeight else noAdaptation
      if isValid then do
        let focus = mkFocus alphabet'
            result :: Big
            result = average $ encodeWithFocus adapt focus input'
            steps = unsafePartial $ decodeSteps adapt isEnd result focus
        H.modify_ (_ { result = Just result, steps = steps })
        else do
        H.modify_ (_ { result = Nothing, steps = Nil })