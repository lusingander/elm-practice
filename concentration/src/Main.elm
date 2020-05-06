module Main exposing (main)

import Browser
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


allCards : List Card
allCards =
    List.range 1 13 |> List.map numberCards |> List.concat


numberCards : Int -> List Card
numberCards =
    numberToRank >> rankCards


rankCards : Rank -> List Card
rankCards r =
    [ Card Spades r
    , Card Hearts r
    , Card Diamonds r
    , Card Clubs r
    ]


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
faceUp s =
    case s of
        CardState c _ ->
            CardState c FaceUp


faceDown : CardState -> CardState
faceDown s =
    case s of
        CardState c _ ->
            CardState c FaceDown


type alias GameState =
    List CardState


type CardState
    = CardState Card FaceState


type FaceState
    = FaceUp
    | FaceDown


type Card
    = Card Suit Rank
    | Joker
    | CardBack


type Suit
    = Spades
    | Hearts
    | Diamonds
    | Clubs


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


numberToRank : Int -> Rank
numberToRank n =
    case n of
        1 ->
            Ace

        2 ->
            Two

        3 ->
            Three

        4 ->
            Four

        5 ->
            Five

        6 ->
            Six

        7 ->
            Seven

        8 ->
            Eight

        9 ->
            Nine

        10 ->
            Ten

        11 ->
            Jack

        12 ->
            Queen

        _ ->
            King


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
        [ style "font-size" "6em"
        , onClick (Open s)
        ]
        [ text (showCardState s) ]


showCardState : CardState -> String
showCardState s =
    case s of
        CardState c FaceUp ->
            showCard c

        CardState _ _ ->
            showCard CardBack


showCard : Card -> String
showCard c =
    case c of
        Card Spades Ace ->
            "🂡"

        Card Spades Two ->
            "🂢"

        Card Spades Three ->
            "🂣"

        Card Spades Four ->
            "🂤"

        Card Spades Five ->
            "🂥"

        Card Spades Six ->
            "🂦"

        Card Spades Seven ->
            "🂧"

        Card Spades Eight ->
            "🂨"

        Card Spades Nine ->
            "🂩"

        Card Spades Ten ->
            "🂪"

        Card Spades Jack ->
            "🂫"

        Card Spades Queen ->
            "🂭"

        Card Spades King ->
            "🂮"

        Card Hearts Ace ->
            "🂱"

        Card Hearts Two ->
            "🂲"

        Card Hearts Three ->
            "🂳"

        Card Hearts Four ->
            "🂴"

        Card Hearts Five ->
            "🂵"

        Card Hearts Six ->
            "🂶"

        Card Hearts Seven ->
            "🂷"

        Card Hearts Eight ->
            "🂸"

        Card Hearts Nine ->
            "🂹"

        Card Hearts Ten ->
            "🂺"

        Card Hearts Jack ->
            "🂻"

        Card Hearts Queen ->
            "🂽"

        Card Hearts King ->
            "🂾"

        Card Diamonds Ace ->
            "🃁"

        Card Diamonds Two ->
            "🃂"

        Card Diamonds Three ->
            "🃃"

        Card Diamonds Four ->
            "🃄"

        Card Diamonds Five ->
            "🃅"

        Card Diamonds Six ->
            "🃆"

        Card Diamonds Seven ->
            "🃇"

        Card Diamonds Eight ->
            "🃈"

        Card Diamonds Nine ->
            "🃉"

        Card Diamonds Ten ->
            "🃊"

        Card Diamonds Jack ->
            "🃋"

        Card Diamonds Queen ->
            "🃍"

        Card Diamonds King ->
            "🃎"

        Card Clubs Ace ->
            "🃑"

        Card Clubs Two ->
            "🃒"

        Card Clubs Three ->
            "🃓"

        Card Clubs Four ->
            "🃔"

        Card Clubs Five ->
            "🃕"

        Card Clubs Six ->
            "🃖"

        Card Clubs Seven ->
            "🃗"

        Card Clubs Eight ->
            "🃘"

        Card Clubs Nine ->
            "🃙"

        Card Clubs Ten ->
            "🃚"

        Card Clubs Jack ->
            "🃛"

        Card Clubs Queen ->
            "🃝"

        Card Clubs King ->
            "🃞"

        Joker ->
            "🃟"

        CardBack ->
            "🂠"
