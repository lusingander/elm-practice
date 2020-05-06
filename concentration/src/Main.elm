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
    { cards : List Card
    , clicked : Card
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model allCards (Card Spades Ace)
    , Cmd.none
    )


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


type Card
    = Card Suit Rank


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
    = New (List Card)
    | Open Card
    | Shuffle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New cards ->
            ( { model | cards = cards }
            , Cmd.none
            )

        Open c ->
            ( { model | clicked = c }
            , Cmd.none
            )

        Shuffle ->
            ( model
            , Random.generate New (shuffle model.cards)
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [] (viewControlArea model :: viewCards model.cards)


viewControlArea : Model -> Html Msg
viewControlArea model =
    div []
        [ viewStatus model
        , button [ onClick Shuffle ] [ text "Reset" ]
        ]


viewStatus : Model -> Html Msg
viewStatus model =
    span []
        [ span [ style "font-size" "1.5em" ] [ text "Last clicked: " ]
        , span [ style "font-size" "5em" ] [ text (showCard model.clicked) ]
        ]


viewCards : List Card -> List (Html Msg)
viewCards cs =
    case cs of
        [] ->
            []

        _ ->
            div []
                (List.map
                    viewCard
                    (List.take 13 cs)
                )
                :: viewCards (List.drop 13 cs)


viewCard : Card -> Html Msg
viewCard c =
    span
        [ style "font-size" "6em"
        , onClick (Open c)
        ]
        [ text (showCard c) ]


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
