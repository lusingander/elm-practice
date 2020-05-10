module Main exposing (main)

import Browser
import Card exposing (Card, allCards, numberEquals, showCard, showCardBack)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Random
import Random.Extra exposing (combine)
import Random.List exposing (shuffle)


main : Program () Model Msg
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


type alias GameState =
    { cardStates : List CardState
    , lastOpened : Maybe Card
    , turn : Turn
    , gameMode : GameMode
    }


type CardState
    = CardState Card FaceState ShowState


type FaceState
    = FaceUp
    | FaceDown
    | TempFaceUp


type Turn
    = First
    | Second
    | Wait


type GameMode
    = Normal
    | Scattered


type alias ShowState =
    { mouseOvered : Bool
    , rotateDeg : Int
    , positionX : Int
    , positionY : Int
    }


initGameState : GameState
initGameState =
    { cardStates = List.map initCardState allCards
    , lastOpened = Nothing
    , turn = First
    , gameMode = Normal
    }


initCardState : Card -> CardState
initCardState c =
    CardState c FaceUp initShowState


initShowState : ShowState
initShowState =
    ShowState False 0 0 0


resetCardStates : List CardState -> List CardState
resetCardStates =
    List.map faceDown


cardFromState : CardState -> Card
cardFromState (CardState c _ _) =
    c


lastOpenedNumberIsEqualTo : Card -> GameState -> Bool
lastOpenedNumberIsEqualTo c gameState =
    let
        lastOpened =
            .lastOpened gameState
    in
    case lastOpened of
        Just n ->
            numberEquals c n

        Nothing ->
            False


faceUpCard : CardState -> GameState -> GameState
faceUpCard originalCardState gameState =
    let
        (CardState card state _) =
            originalCardState
    in
    if .turn gameState == First && state == FaceDown then
        { gameState
            | cardStates = faceUpSingleCard originalCardState gameState.cardStates
            , lastOpened = Just (cardFromState originalCardState)
            , turn = Second
        }

    else if .turn gameState == Second && state == FaceDown then
        if lastOpenedNumberIsEqualTo card gameState then
            { gameState
                | cardStates = faceUpSingleCard originalCardState gameState.cardStates |> faceUpAllTempFaceUpCards
                , lastOpened = Nothing
                , turn = First
            }

        else
            { gameState
                | cardStates = faceUpSingleCard originalCardState gameState.cardStates
                , lastOpened = Just (cardFromState originalCardState)
                , turn = Wait
            }

    else if .turn gameState == Wait then
        { gameState
            | cardStates = .cardStates gameState |> faceDownAllTempFaceUpCards
            , lastOpened = Nothing
            , turn = First
        }

    else
        gameState


faceUpSingleCard : CardState -> List CardState -> List CardState
faceUpSingleCard originalCardState gameState =
    case gameState of
        [] ->
            []

        s :: ss ->
            (if originalCardState == s then
                faceUp s

             else
                s
            )
                :: faceUpSingleCard originalCardState ss


faceUp : CardState -> CardState
faceUp (CardState c _ s) =
    CardState c TempFaceUp s


faceDown : CardState -> CardState
faceDown (CardState c _ s) =
    CardState c FaceDown s


faceUpAllTempFaceUpCards : List CardState -> List CardState
faceUpAllTempFaceUpCards cardStates =
    case cardStates of
        [] ->
            []

        s :: ss ->
            let
                (CardState card face show) =
                    s
            in
            (if face == TempFaceUp then
                CardState card FaceUp show

             else
                s
            )
                :: faceUpAllTempFaceUpCards ss


faceDownAllTempFaceUpCards : List CardState -> List CardState
faceDownAllTempFaceUpCards cardStates =
    case cardStates of
        [] ->
            []

        s :: ss ->
            let
                (CardState card face show) =
                    s
            in
            (if face == TempFaceUp then
                CardState card FaceDown show

             else
                s
            )
                :: faceDownAllTempFaceUpCards ss


type Msg
    = New (List CardState)
    | Open CardState
    | MouseEnter CardState
    | MouseLeave CardState
    | Shuffle GameMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New cardStates ->
            ( { model
                | cardStates = resetCardStates cardStates
                , lastOpened = Nothing
                , turn = First
              }
            , Cmd.none
            )

        Open cardState ->
            ( faceUpCard cardState model
            , Cmd.none
            )

        MouseEnter cardState ->
            ( { model | cardStates = mouseEnterToCard cardState <| .cardStates model }
            , Cmd.none
            )

        MouseLeave cardState ->
            ( { model | cardStates = mouseLeaveFromCard cardState <| .cardStates model }
            , Cmd.none
            )

        Shuffle gm ->
            ( { model
                | gameMode = gm
              }
            , Random.generate New (Random.andThen shuffle <| randomCardStates model.cardStates)
            )


randomCardStates : List CardState -> Random.Generator (List CardState)
randomCardStates cs =
    List.map randomCardState cs |> Random.Extra.combine


randomCardState : CardState -> Random.Generator CardState
randomCardState (CardState c f s) =
    Random.map (CardState c f) (randomShowState <| .mouseOvered s)


randomShowState : Bool -> Random.Generator ShowState
randomShowState mouseOvered =
    Random.map3 (ShowState mouseOvered) randomDegree (randomPosition 30) (randomPosition 100)


randomDegree : Random.Generator Int
randomDegree =
    Random.int 0 359


randomPosition : Int -> Random.Generator Int
randomPosition max =
    Random.int 0 max


mouseEnterToCard : CardState -> List CardState -> List CardState
mouseEnterToCard s cs =
    updateMouserOveredState True s cs


mouseLeaveFromCard : CardState -> List CardState -> List CardState
mouseLeaveFromCard s cs =
    updateMouserOveredState False s cs


updateMouserOveredState : Bool -> CardState -> List CardState -> List CardState
updateMouserOveredState mouseOvered originalCardState cardStates =
    case cardStates of
        [] ->
            []

        x :: xs ->
            (if originalCardState == x then
                let
                    (CardState c f s) =
                        originalCardState
                in
                CardState c f { s | mouseOvered = mouseOvered }

             else
                x
            )
                :: updateMouserOveredState mouseOvered originalCardState xs


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [] (viewControlArea model :: viewCards model.gameMode model.cardStates)


viewControlArea : Model -> Html Msg
viewControlArea _ =
    div []
        [ button [ onClick <| Shuffle Normal ] [ text "Reset" ]
        , button [ onClick <| Shuffle Scattered ] [ text "Reset (Scattered)" ]
        ]


viewCards : GameMode -> List CardState -> List (Html Msg)
viewCards gm ss =
    case ss of
        [] ->
            []

        _ ->
            div []
                (List.map
                    (viewCard gm)
                    (List.take 13 ss)
                )
                :: viewCards gm (List.drop 13 ss)


viewCard : GameMode -> CardState -> Html Msg
viewCard gm s =
    span
        (onClick (Open s)
            :: onMouseEnter (MouseEnter s)
            :: onMouseLeave (MouseLeave s)
            :: cardStyle gm s
        )
        [ text (showCardState s) ]


cardStyle : GameMode -> CardState -> List (Html.Attribute msg)
cardStyle gm (CardState _ f s) =
    [ style "font-size" "6em"
    , style "user-select" "none"
    ]
        ++ (if f == TempFaceUp then
                [ style "color" "crimson" ]

            else if f == FaceDown then
                [ style "cursor" "pointer" ]

            else
                []
           )
        ++ (if .mouseOvered s && f == FaceDown then
                [ style "color" "skyblue" ]

            else
                []
           )
        ++ (if gm == Scattered then
                [ style "transform" <| transformStyleString (.rotateDeg s) (.positionX s) (.positionY s)
                , style "display" "inline-block"
                ]

            else
                []
           )


transformStyleString : Int -> Int -> Int -> String
transformStyleString d x y =
    "translate(" ++ String.fromInt x ++ "px, " ++ String.fromInt y ++ "px) rotate(" ++ String.fromInt d ++ "deg)"


showCardState : CardState -> String
showCardState s =
    case s of
        CardState _ FaceDown _ ->
            showCardBack

        CardState c _ _ ->
            showCard c
