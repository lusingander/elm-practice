module Main exposing (main)

import Array
import Array.Extra
import Browser
import Dict
import Example
import Html exposing (Html, button, code, div, option, pre, select, span, text, textarea)
import Html.Attributes exposing (style, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode
import List.Extra
import String exposing (fromInt)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { memory : Memory
    , pointer : Pointer
    , currentStep : Int
    , inputAreaText : String
    , source : String
    , output : String
    , jumpInfo : JumpInfo
    , status : ExecutionStatus
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


type ExecutionStatus
    = Default
    | Executing
    | AutoPlaying
    | End


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel
    , Cmd.none
    )


initModel : Model
initModel =
    { memory = initMemory
    , pointer = initPointer
    , currentStep = initCurrentStep
    , inputAreaText = ""
    , source = ""
    , output = ""
    , jumpInfo = []
    , status = Default
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
    buildJumpInfoStack source 0 [] []


buildJumpInfoStack : String -> Int -> JumpInfo -> JumpInfo -> JumpInfo
buildJumpInfoStack source index tmpStack retStack =
    if index >= String.length source then
        retStack

    else
        case indexChar index source of
            "[" ->
                let
                    newTmpStack =
                        ( index, 0 ) :: tmpStack
                in
                buildJumpInfoStack source (index + 1) newTmpStack retStack

            "]" ->
                let
                    newHead =
                        List.head tmpStack
                            |> Maybe.map (\t -> ( Tuple.first t, index ))

                    newRetStack =
                        Maybe.map (\t -> t :: retStack) newHead
                            |> Maybe.withDefault []
                in
                buildJumpInfoStack source (index + 1) (List.drop 1 tmpStack) newRetStack

            _ ->
                buildJumpInfoStack source (index + 1) tmpStack retStack


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
        , status = Executing
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
        Just IncrementPointer ->
            { model | pointer = cp + 1 }
                |> incrementCurrentStep

        Just DecrementPointer ->
            { model | pointer = cp - 1 }
                |> incrementCurrentStep

        Just IncrementValue ->
            { model | memory = incrementArrayValue cp cm }
                |> incrementCurrentStep

        Just DecrementValue ->
            { model | memory = decrementArrayValue cp cm }
                |> incrementCurrentStep

        Just Output ->
            { model | output = .output model ++ outputPointerByteString cp cm }
                |> incrementCurrentStep

        Just Input ->
            -- Not implemented
            model

        Just JumpForward ->
            case Array.get cp cm of
                Just 0 ->
                    { model
                        | currentStep =
                            searchForwardJumpPosition cs (.jumpInfo model)
                                |> getNextValidCharIndex (.source model)
                    }

                _ ->
                    incrementCurrentStep model

        Just JumpBack ->
            { model
                | currentStep = searchPreviousJumpPosition cs (.jumpInfo model)
            }

        Nothing ->
            model


incrementCurrentStep : Model -> Model
incrementCurrentStep model =
    let
        updated =
            { model | currentStep = .currentStep model + 1 }
    in
    if eof updated then
        { model | status = End }

    else if currentIsValidChar updated then
        updated

    else
        incrementCurrentStep updated


eof : Model -> Bool
eof model =
    endOfSource (.currentStep model) (.source model)


endOfSource : Int -> String -> Bool
endOfSource current source =
    String.length source < current


searchForwardJumpPosition : Int -> JumpInfo -> Int
searchForwardJumpPosition current jumpList =
    List.Extra.find (\t -> Tuple.first t == current) jumpList
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0


searchPreviousJumpPosition : Int -> JumpInfo -> Int
searchPreviousJumpPosition current jumpList =
    List.Extra.find (\t -> Tuple.second t == current) jumpList
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


getNextValidCharIndex : String -> Int -> Int
getNextValidCharIndex source current =
    let
        next =
            current + 1
    in
    if endOfSource next source then
        next

    else if indexIsValidChar next source then
        next

    else
        getNextValidCharIndex source next


incrementArrayValue : Int -> Memory -> Memory
incrementArrayValue index =
    Array.Extra.update index ((+) 1)


decrementArrayValue : Int -> Memory -> Memory
decrementArrayValue index =
    Array.Extra.update index (\n -> n - 1)


outputPointerByteString : Int -> Memory -> String
outputPointerByteString index array =
    Array.get index array
        |> Maybe.andThen ascii
        |> Maybe.withDefault ""


currentCommand : Int -> Model -> Maybe Command
currentCommand currentStep =
    stringToCommand << indexChar currentStep << .source


indexChar : Int -> String -> String
indexChar index str =
    String.dropLeft index str |> String.left 1


currentIsValidChar : Model -> Bool
currentIsValidChar model =
    indexIsValidChar (.currentStep model) (.source model)


indexIsValidChar : Int -> String -> Bool
indexIsValidChar current =
    isValidChar << indexChar current


isValidChar : String -> Bool
isValidChar str =
    List.member str [ ">", "<", "+", "-", ".", ",", "[", "]" ]


stringToCommand : String -> Maybe Command
stringToCommand s =
    case s of
        ">" ->
            Just IncrementPointer

        "<" ->
            Just DecrementPointer

        "+" ->
            Just IncrementValue

        "-" ->
            Just DecrementValue

        "." ->
            Just Output

        "," ->
            Just Input

        "[" ->
            Just JumpForward

        "]" ->
            Just JumpBack

        _ ->
            Nothing


type Msg
    = Start
    | StepNext
    | InputAreaUpdate String
    | Tick
    | Play
    | Pause
    | ExampleSelectChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( start model
            , Cmd.none
            )

        StepNext ->
            ( step model
            , Cmd.none
            )

        InputAreaUpdate input ->
            ( { model
                | inputAreaText = input
                , output = ""
                , source = ""
                , status = Default
              }
            , Cmd.none
            )

        Tick ->
            if .status model == AutoPlaying then
                ( step model
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        Play ->
            if .status model == Executing then
                ( { model
                    | status = AutoPlaying
                  }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        Pause ->
            if .status model == AutoPlaying then
                ( { model
                    | status = Executing
                  }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        ExampleSelectChange select ->
            update (InputAreaUpdate <| Example.getExampleString select) model


subscriptions : Model -> Sub Msg
subscriptions model =
    if .status model == AutoPlaying then
        Time.every 100 (\_ -> Tick)

    else
        Sub.none


view : Model -> Html Msg
view model =
    div
        [ style "margin" "30px" ]
        [ viewExampleSelect
        , viewStatus (.memory model) (.pointer model) (.status model)
        , viewInputArea (.inputAreaText model)
        , viewOutputArea (.output model)
        , viewShowArea (.source model) (.currentStep model)
        , viewDebugStates model
        ]


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" (Json.Decode.map handler Html.Events.targetValue)


viewExampleSelect : Html Msg
viewExampleSelect =
    let
        handler selected =
            ExampleSelectChange selected
    in
    select
        [ onChange handler
        ]
        (List.map
            (\( v, t ) -> option [ value v ] [ text t ])
            Example.examples
        )


viewStatus : Memory -> Pointer -> ExecutionStatus -> Html Msg
viewStatus memory pointer status =
    div []
        [ viewMemory memory pointer
        , div [] [ text <| "pointer: " ++ fromInt pointer ]
        , button [ onClick Start ] [ text "Start" ]
        , button [ onClick StepNext ] [ text "Next" ]
        , viewPlayButton status
        ]


viewPlayButton : ExecutionStatus -> Html Msg
viewPlayButton status =
    if status == AutoPlaying then
        button [ onClick Pause ] [ text "Pause" ]

    else
        button [ onClick Play ] [ text "Play" ]


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


viewInputArea : String -> Html Msg
viewInputArea input =
    div []
        [ div [] [ text "Input:" ]
        , textarea
            (inputAreaStyles
                ++ [ onInput InputAreaUpdate
                   , value input
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
        , pre
            [ style "border" "1px solid"
            ]
            [ code
                inputAreaStyles
                [ span [] [ text prevString ]
                , span
                    [ style "color" "red"
                    ]
                    [ text currentChar ]
                , span [] [ text forwardString ]
                ]
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


viewDebugStates : Model -> Html Msg
viewDebugStates model =
    div []
        [ div []
            [ text <| "Current Step: " ++ (String.fromInt <| .currentStep model) ]
        , div []
            [ text <| "Current State: " ++ (executionStatusToString <| .status model) ]
        ]


executionStatusToString : ExecutionStatus -> String
executionStatusToString status =
    case status of
        Default ->
            "Default"

        Executing ->
            "Executing"

        AutoPlaying ->
            "AutoPlaying"

        End ->
            "End"


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
