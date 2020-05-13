module Main exposing (main)

import Array
import Array.Extra
import Browser
import Dict
import Html exposing (Html, button, div, span, text, textarea)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick, onInput)
import List.Extra
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
    , jumpInfo : JumpInfo
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


type alias JumpInfo =
    List ( Int, Int )


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
    , jumpInfo = []
    }


initMemory : Memory
initMemory =
    Array.repeat 20 0


initPointer : Pointer
initPointer =
    0


initCurrentStep : Int
initCurrentStep =
    0


buildJumpInfo : String -> JumpInfo
buildJumpInfo source =
    buildJumpInfoStack source 0 (String.length source) [] []


buildJumpInfoStack : String -> Int -> Int -> JumpInfo -> JumpInfo -> JumpInfo
buildJumpInfoStack source index length tmpStack retStack =
    if index >= length then
        retStack

    else
        case indexChar index source of
            "[" ->
                let
                    newTmpStack =
                        ( index, 0 ) :: tmpStack
                in
                buildJumpInfoStack source (index + 1) length newTmpStack retStack

            "]" ->
                let
                    newHead =
                        List.head tmpStack
                            |> Maybe.map (\t -> ( Tuple.first t, index ))

                    newRetStack =
                        Maybe.map (\t -> t :: retStack) newHead
                            |> Maybe.withDefault []
                in
                buildJumpInfoStack source (index + 1) length (List.drop 1 tmpStack) newRetStack

            _ ->
                buildJumpInfoStack source (index + 1) length tmpStack retStack


start : Model -> Model
start model =
    let
        source =
            .inputAreaText model
    in
    { model
        | memory = initMemory
        , pointer = initPointer
        , currentStep = initCurrentStep
        , source = source
        , output = ""
        , jumpInfo = buildJumpInfo source
    }


step : Model -> Model
step model =
    let
        cs =
            .currentStep model

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
                | currentStep = cs + 1
                , pointer = cp + 1
            }

        DecrementPointer ->
            { model
                | currentStep = cs + 1
                , pointer = cp - 1
            }

        IncrementValue ->
            { model
                | currentStep = cs + 1
                , memory = incrementArrayValue cp cm
            }

        DecrementValue ->
            { model
                | currentStep = cs + 1
                , memory = decrementArrayValue cp cm
            }

        Output ->
            { model
                | currentStep = cs + 1
                , output = .output model ++ outputPointerByteString cp cm
            }

        Input ->
            -- Not implemented
            model

        JumpForward ->
            case Array.get cp cm of
                Just 0 ->
                    { model
                        | currentStep = searchForwardJumpPosition cs (.jumpInfo model)
                    }

                _ ->
                    { model
                        | currentStep = cs + 1
                    }

        JumpBack ->
            { model
                | currentStep = searchPreviousJumpPosition cs (.jumpInfo model)
            }


searchForwardJumpPosition : Int -> JumpInfo -> Int
searchForwardJumpPosition current jumpList =
    List.Extra.find (\t -> Tuple.first t == current) jumpList
        |> Maybe.map Tuple.second
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0


searchPreviousJumpPosition : Int -> JumpInfo -> Int
searchPreviousJumpPosition current jumpList =
    List.Extra.find (\t -> Tuple.second t == current) jumpList
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


incrementArrayValue : Int -> Memory -> Memory
incrementArrayValue index array =
    Array.Extra.update index ((+) 1) array


decrementArrayValue : Int -> Memory -> Memory
decrementArrayValue index array =
    Array.Extra.update index (\n -> n - 1) array


outputPointerByteString : Int -> Memory -> String
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
                , output = ""
                , source = ""
            }


view : Model -> Html Msg
view model =
    div
        [ style "margin" "30px" ]
        [ viewStatus (.memory model) (.pointer model)
        , viewInputArea
        , viewOutputArea (.output model)
        , viewShowArea (.source model) (.currentStep model)
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
            [ ( 9, "\t" )
            , ( 10, "\n" )
            , ( 32, " " )
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
