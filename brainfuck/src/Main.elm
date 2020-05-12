module Main exposing (main)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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
    Model initMemory initPointer


initMemory : Memory
initMemory =
    List.repeat 20 0


initPointer : Pointer
initPointer =
    0


type Msg
    = StepNext


update : Msg -> Model -> Model
update msg model =
    case msg of
        _ ->
            model


view : Model -> Html Msg
view model =
    div
        [ style "margin" "30px" ]
        [ viewStatus (.memory model) (.pointer model) ]


viewStatus : Memory -> Pointer -> Html Msg
viewStatus memory pointer =
    div []
        [ viewMemory memory pointer
        , div [] [ text <| "pointer: " ++ fromInt pointer ]
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
