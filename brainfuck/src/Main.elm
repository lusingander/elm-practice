module Main exposing (main)

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
    }


type alias Memory =
    List Int


type alias Pointer =
    Int


init : Model
init =
    initModel


initModel : Model
initModel =
    { memory = initMemory
    , pointer = initPointer
    , currentStep = 0
    , inputAreaText = ""
    }


initMemory : Memory
initMemory =
    List.repeat 20 0


initPointer : Pointer
initPointer =
    0


type Msg
    = Start
    | StepNext
    | InputAreaUpdate String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Start ->
            model

        StepNext ->
            { model
                | currentStep = .currentStep model + 1
            }

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
    span [] <| List.indexedMap (\i m -> viewSingleMemory (i == pointer) m) memory


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
