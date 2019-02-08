module UI where

import ArithmeticCoding
import ArithmeticCoding.Chr
import FileSaver
import Halogen.FileInputComponent as FI

import CSS (backgroundColor, color, grey, marginLeft, px, rgba, width)
import Data.Argonaut.Core (fromObject, toObject, stringify)
import Data.Argonaut.Core as AC
import Data.Argonaut.Parser (jsonParser)
import Data.Array as A
import Data.Big (Big, fromString)
import Data.Either (hush)
import Data.Int (toNumber)
import Data.List (List(..), all, elem)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.MediaType (MediaType(..))
import Data.String (CodePoint, fromCodePointArray, toCodePointArray)
import Data.String.CodePoints as CP
import Data.Tuple (Tuple (..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (lookup)
import Foreign.Object as StrMap
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, type (~>), Unit, bind, const, discard, flip, map, pure, show, unit, when, zero, ($), (*), (-), (/), (<<<), (<>), (==), (>=>))


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
  | UpdateFileInput FI.Message a
  | SaveToFile a


data Slot = FileInputSlot

derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


type Message = Unit


ui :: H.Component HH.HTML Query Unit Unit Aff
ui = H.parentComponent
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

  render :: State -> H.ParentHTML Query FI.Query Slot Aff
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

    , HH.br_

    , HH.label
      [ HP.title "Calculate result value and save to file"
      , HE.onClick $ HE.input_ SaveToFile
      , HP.class_ $ HH.ClassName "file-buttons"
      ]
      [ HH.text "Save to file" ]
    , HH.text " / "
    , HH.label
      [ HP.title "Load result from file"
      , HP.class_ $ HH.ClassName "file-buttons" ]
      [ HH.slot FileInputSlot
                (FI.component { componentId: "fileinput"
                              , isBinary: false
                              , prompt: "Load from file"
                              , accept: MediaType ".json"
                              })
                unit
                (HE.input UpdateFileInput)
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
             , HH.text $ show result' ]
           , HH.div [ HP.id_ "container" ] <<< A.fromFoldable $
             map (HH.div_ <<< renderStep) steps
           ]
         Nothing ->
           [ HH.br_, HH.text "Invalid input: some characters are not in alphabet!" ]
      ] else []


  mkBounds { lowerBound, upperBound } =
    HH.div [ HP.id_ "bounds-container" ]
    [ HH.div [ HP.classes [ HH.ClassName "one-line", HH.ClassName "lower-bound" ] ]
      [ HH.text $ show lowerBound ]

    , HH.div [ HP.classes [ HH.ClassName "one-line", HH.ClassName "upper-bound" ] ]
      [ HH.text $ show upperBound ]
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
           [ HH.text $ "width = " <> show interval
           ]
         ]
    else []


  eval :: Query ~> H.ParentDSL State Query FI.Query Slot Message Aff
  eval = case _ of
    (UpdateFileInput (FI.FileLoaded { contents, name }) next) -> do
      let warn = liftEffect $ log "Incorrect file!"
      maybe warn
            (\{ output, alphabet, adaptive } -> do
              case fromString output of
                Nothing -> warn
                Just result -> do
                  -- restore input
                  let alphabet' = wrap $ A.nub $ toCodePointArray alphabet
                      focus = mkFocus alphabet'
                      adapt = if adaptive then increaseWeight else noAdaptation
                      steps :: List (StepInfo (Chr CodePoint))
                      steps = unsafePartial $ decodeSteps adapt isEnd result focus
                      input :: String
                      input = fromCodePointArray $ unwrap $ A.fromFoldable $ map (_.result) steps
                  H.modify_ (_ { input = input, alphabet = alphabet, adaptive = adaptive })
                  processInput)
            (parse contents)
      pure next
    (SaveToFile next) -> do
      processInput --  commit changes
      state <- H.get
      maybe (pure unit)
            (\big -> do
                -- construct and serialize a JSON object
                let obj = fromObject $ StrMap.fromFoldable
                          [ Tuple "output"   (AC.fromString (show big))
                          , Tuple "alphabet" (AC.fromString state.alphabet)
                          , Tuple "adaptive" (AC.fromBoolean state.adaptive)
                          ]
                liftEffect $ saveAsPlainText (show state.input <> ".json")
                                             (stringify obj))
            state.result
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
      parse :: String -> Maybe { output :: String
                               , alphabet :: String
                               , adaptive :: Boolean }
      parse str = do
        json <- hush (jsonParser str)
        obj <- toObject json
        -- lookup key and convert inside Maybe
        let getStrKey     key = lookup key >=> AC.toString
            getBooleanKey key = lookup key >=> AC.toBoolean
        output   <- getStrKey "output"   obj
        alphabet <- getStrKey "alphabet" obj
        adaptive <- getBooleanKey "adaptive" obj
        pure { output, alphabet, adaptive }

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
              result = fromMaybe zero $ fromString $ show $
                average $ encodeWithFocus adapt focus input'
              steps = unsafePartial $ decodeSteps adapt isEnd result focus
              success :: Boolean
              success = A.fromFoldable (map _.result steps) == input'
          H.modify_ (_ { result = Just result, steps = steps, success = success })
          else do
          H.modify_ (_ { result = Nothing, steps = Nil })
