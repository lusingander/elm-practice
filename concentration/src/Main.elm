module Main exposing (main)

import Browser
import Card exposing (Card, allCards, showCard)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Random.List exposing (shuffle)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    GameState


init : () -> ( Model, Cmd Msg )
init _ =
    ( initGameState
    , Cmd.none
    )


initGameState : GameState
initGameState =
    List.map initCardState allCards


initCardState : Card -> CardState
initCardState c =
    CardState c FaceDown


resetCardStates : List CardState -> List CardState
resetCardStates =
    List.map faceDown


faceUpCard : CardState -> GameState -> GameState
faceUpCard originalCardState gameState =
    case gameState of
        [] ->
            []

        s :: ss ->
            (if originalCardState == s then
                faceUp s

             else
                s
            )
                :: faceUpCard originalCardState ss


faceUp : CardState -> CardState
faceUp (CardState c _) =
    CardState c TempFaceUp


faceDown : CardState -> CardState
faceDown (CardState c _) =
    CardState c FaceDown


type alias GameState =
    List CardState


type CardState
    = CardState Card FaceState


type FaceState
    = FaceUp
    | FaceDown
    | TempFaceUp


type Msg
    = New (List CardState)
    | Open CardState
    | Shuffle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New cardStates ->
            ( resetCardStates cardStates
            , Cmd.none
            )

        Open cardState ->
            ( faceUpCard cardState model
            , Cmd.none
            )

        Shuffle ->
            ( model
            , Random.generate New (shuffle model)
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [] (viewControlArea model :: viewCards model)


viewControlArea : Model -> Html Msg
viewControlArea _ =
    div []
        [ button [ onClick Shuffle ] [ text "Reset" ]
        ]


viewCards : List CardState -> List (Html Msg)
viewCards ss =
    case ss of
        [] ->
            []

        _ ->
            div []
                (List.map
                    viewCard
                    (List.take 13 ss)
                )
                :: viewCards (List.drop 13 ss)


viewCard : CardState -> Html Msg
viewCard s =
    span
        (onClick
            (Open s)
            :: cardStyle s
        )
        [ text (showCardState s) ]


cardStyle : CardState -> List (Html.Attribute msg)
cardStyle (CardState _ s) =
    (if s == TempFaceUp then
        [ style "color" "crimson" ]

     else
        []
    )
        ++ [ style "font-size" "6em" ]


showCardState : CardState -> String
showCardState s =
    case s of
        CardState c FaceDown ->
            showCard False c

        CardState c _ ->
            showCard True c
