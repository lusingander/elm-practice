module Main exposing (main)

import Array
import Browser
import Dict
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
    , output : String
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
    , output = ""
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
        , output = ""
    }


step : Model -> Model
step model =
    let
        cs =
            .currentStep model + 1

        cp =
            .pointer model

        cm =
            .memory model

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
                , memory = incrementArrayValue cp cm
            }

        DecrementValue ->
            { model
                | currentStep = cs
                , memory = decrementArrayValue cp cm
            }

        Output ->
            { model
                | currentStep = cs
                , output = .output model ++ outputPointerByteString cp cm
            }

        Input ->
            model

        JumpForward ->
            model

        JumpBack ->
            model


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


outputPointerByteString : Int -> Array.Array Int -> String
outputPointerByteString index array =
    Array.get index array
        |> Maybe.andThen ascii
        |> Maybe.withDefault ""


currentCommand : Int -> Model -> Command
currentCommand currentStep model =
    .source model
        |> String.dropLeft currentStep
        |> String.left 1
        |> stringToCommand


indexChar : Int -> String -> String
indexChar index str =
    String.dropLeft index str |> String.left 1


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
        , viewOutputArea (.output model)
        , viewShowArea (.inputAreaText model) (.currentStep model)
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
        [ div [] [ text "Input:" ]
        , textarea
            (inputAreaStyles
                ++ [ onInput InputAreaUpdate
                   ]
            )
            []
        ]


viewShowArea : String -> Int -> Html Msg
viewShowArea input current =
    let
        ( prevString, currentChar, forwardString ) =
            if current >= 0 then
                ( String.left current input
                , indexChar current input
                , String.dropLeft (current + 1) input
                )

            else
                ( input, "", "" )
    in
    div []
        [ div [] [ text "Visual:" ]
        , div
            (inputAreaStyles
                ++ [ style "border" "1px solid"
                   , style "word-break" "break-all"
                   , disabled True
                   ]
            )
            [ span [] [ text prevString ]
            , span
                [ style "color" "red"
                ]
                [ text currentChar ]
            , span [] [ text forwardString ]
            ]
        ]


inputAreaStyles : List (Html.Attribute Msg)
inputAreaStyles =
    [ style "width" "600px"
    , style "height" "200px"
    , style "font-size" "20px"
    , style "font-family" "\"Courier New\", monospace"
    ]


viewOutputArea : String -> Html Msg
viewOutputArea output =
    div []
        [ text <| "Output: " ++ output
        ]


ascii : Int -> Maybe String
ascii n =
    Dict.get n <|
        Dict.fromList
            [ ( 32, "Space" )
            , ( 33, "!" )
            , ( 34, "\"" )
            , ( 35, "#" )
            , ( 36, "$" )
            , ( 37, "%" )
            , ( 38, "&" )
            , ( 39, "'" )
            , ( 40, "(" )
            , ( 41, ")" )
            , ( 42, "*" )
            , ( 43, "+" )
            , ( 44, "," )
            , ( 45, "-" )
            , ( 46, "." )
            , ( 47, "/" )
            , ( 48, "0" )
            , ( 49, "1" )
            , ( 50, "2" )
            , ( 51, "3" )
            , ( 52, "4" )
            , ( 53, "5" )
            , ( 54, "6" )
            , ( 55, "7" )
            , ( 56, "8" )
            , ( 57, "9" )
            , ( 58, ":" )
            , ( 59, ";" )
            , ( 60, "<" )
            , ( 61, "=" )
            , ( 62, ">" )
            , ( 63, "?" )
            , ( 64, "@" )
            , ( 65, "A" )
            , ( 66, "B" )
            , ( 67, "C" )
            , ( 68, "D" )
            , ( 69, "E" )
            , ( 70, "F" )
            , ( 71, "G" )
            , ( 72, "H" )
            , ( 73, "I" )
            , ( 74, "J" )
            , ( 75, "K" )
            , ( 76, "L" )
            , ( 77, "M" )
            , ( 78, "N" )
            , ( 79, "O" )
            , ( 80, "P" )
            , ( 81, "Q" )
            , ( 82, "R" )
            , ( 83, "S" )
            , ( 84, "T" )
            , ( 85, "U" )
            , ( 86, "V" )
            , ( 87, "W" )
            , ( 88, "X" )
            , ( 89, "Y" )
            , ( 90, "Z" )
            , ( 91, "[" )
            , ( 92, "\\" )
            , ( 93, "]" )
            , ( 94, "^" )
            , ( 95, "_" )
            , ( 96, "`" )
            , ( 97, "a" )
            , ( 98, "b" )
            , ( 99, "c" )
            , ( 100, "d" )
            , ( 101, "e" )
            , ( 102, "f" )
            , ( 103, "g" )
            , ( 104, "h" )
            , ( 105, "i" )
            , ( 106, "j" )
            , ( 107, "k" )
            , ( 108, "l" )
            , ( 109, "m" )
            , ( 110, "n" )
            , ( 111, "o" )
            , ( 112, "p" )
            , ( 113, "q" )
            , ( 114, "r" )
            , ( 115, "s" )
            , ( 116, "t" )
            , ( 117, "u" )
            , ( 118, "v" )
            , ( 119, "w" )
            , ( 120, "x" )
            , ( 121, "y" )
            , ( 122, "z" )
            , ( 123, "{" )
            , ( 124, "|" )
            , ( 125, "}" )
            , ( 126, "~" )
            ]
