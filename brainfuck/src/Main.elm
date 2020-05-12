module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, span, text, textarea)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { memory : Memory
    , pointer : Pointer
    , currentStep : Int
    , inputAreaText : String
    , source : String
    }


type alias Memory =
    Array.Array Int


type alias Pointer =
    Int


type Command
    = IncrementPointer
    | DecrementPointer
    | IncrementValue
    | DecrementValue
    | Output
    | Input
    | JumpForward
    | JumpBack


init : Model
init =
    initModel


initModel : Model
initModel =
    { memory = initMemory
    , pointer = initPointer
    , currentStep = initCurrentStep
    , inputAreaText = ""
    , source = ""
    }


initMemory : Memory
initMemory =
    Array.repeat 20 0


initPointer : Pointer
initPointer =
    0


initCurrentStep : Int
initCurrentStep =
    -1


start : Model -> Model
start model =
    { model
        | memory = initMemory
        , pointer = initPointer
        , currentStep = initCurrentStep
        , source = .inputAreaText model
    }


step : Model -> Model
step model =
    let
        cs =
            .currentStep model + 1

        cp =
            .pointer model

        command =
            currentCommand cs model
    in
    case command of
        IncrementPointer ->
            { model
                | currentStep = cs
                , pointer = cp + 1
            }

        DecrementPointer ->
            { model
                | currentStep = cs
                , pointer = cp - 1
            }

        IncrementValue ->
            { model
                | currentStep = cs
                , memory = incrementArrayValue cp (.memory model)
            }

        DecrementValue ->
            { model
                | currentStep = cs
                , memory = decrementArrayValue cp (.memory model)
            }

        _ ->
            { model
                | currentStep = cs + 1
            }


incrementArrayValue : Int -> Array.Array Int -> Array.Array Int
incrementArrayValue index array =
    case Array.get index array of
        Just v ->
            Array.set index (v + 1) array

        Nothing ->
            array


decrementArrayValue : Int -> Array.Array Int -> Array.Array Int
decrementArrayValue index array =
    case Array.get index array of
        Just v ->
            Array.set index (v - 1) array

        Nothing ->
            array


currentCommand : Int -> Model -> Command
currentCommand currentStep model =
    .source model
        |> String.dropLeft currentStep
        |> String.left 1
        |> stringToCommand


stringToCommand : String -> Command
stringToCommand s =
    case s of
        ">" ->
            IncrementPointer

        "<" ->
            DecrementPointer

        "+" ->
            IncrementValue

        "-" ->
            DecrementValue

        "." ->
            Output

        "[" ->
            JumpForward

        "]" ->
            JumpBack

        _ ->
            Input


type Msg
    = Start
    | StepNext
    | InputAreaUpdate String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Start ->
            start model

        StepNext ->
            step model

        InputAreaUpdate input ->
            { model
                | inputAreaText = input
            }


view : Model -> Html Msg
view model =
    div
        [ style "margin" "30px" ]
        [ viewStatus (.memory model) (.pointer model)
        , viewInputArea
        , viewShowArea (.inputAreaText model)
        ]


viewStatus : Memory -> Pointer -> Html Msg
viewStatus memory pointer =
    div []
        [ viewMemory memory pointer
        , div [] [ text <| "pointer: " ++ fromInt pointer ]
        , button [ onClick Start ] [ text "Start" ]
        , button [ onClick StepNext ] [ text "Next" ]
        ]


viewMemory : Memory -> Pointer -> Html Msg
viewMemory memory pointer =
    span [] <|
        Array.toList <|
            Array.indexedMap (\i m -> viewSingleMemory (i == pointer) m) memory


viewSingleMemory : Bool -> Int -> Html Msg
viewSingleMemory current n =
    span
        ([ style "border" "1px solid"
         , style "margin" "1px"
         ]
            ++ (if current then
                    [ style "color" "crimson" ]

                else
                    []
               )
        )
        [ text <| fromInt n ]


viewInputArea : Html Msg
viewInputArea =
    div []
        [ textarea
            (inputAreaStyles
                ++ [ onInput InputAreaUpdate
                   ]
            )
            []
        ]


viewShowArea : String -> Html Msg
viewShowArea input =
    div []
        [ textarea
            (inputAreaStyles
                ++ [ style "border" "1px solid"
                   , disabled True
                   ]
            )
            [ text input
            ]
        ]


inputAreaStyles : List (Html.Attribute Msg)
inputAreaStyles =
    [ style "width" "300px"
    , style "height" "100px"
    , style "font-size" "20px"
    , style "font-family" "\"Courier New\", monospace"
    ]
