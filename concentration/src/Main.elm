module Main exposing (main)

import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    List Card


init : Model
init =
    allCards


allCards : List Card
allCards =
    List.concat (List.map numberCards (List.range 1 13))


numberCards : Int -> List Card
numberCards n =
    [ Card Spades n, Card Hearts n, Card Diamonds n, Card Clubs n ]


type Msg
    = Open


type Suit
    = Spades
    | Hearts
    | Diamonds
    | Clubs


type Card
    = Card Suit Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Open ->
            model


view : Model -> Html Msg
view model =
    div [] (viewCards model)


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
    span [ style "font-size" "6em" ] [ text (showCard c) ]


showCard : Card -> String
showCard c =
    case c of
        Card Spades 1 ->
            "🂡"

        Card Spades 2 ->
            "🂢"

        Card Spades 3 ->
            "🂣"

        Card Spades 4 ->
            "🂤"

        Card Spades 5 ->
            "🂥"

        Card Spades 6 ->
            "🂦"

        Card Spades 7 ->
            "🂧"

        Card Spades 8 ->
            "🂨"

        Card Spades 9 ->
            "🂩"

        Card Spades 10 ->
            "🂪"

        Card Spades 11 ->
            "🂫"

        Card Spades 12 ->
            "🂭"

        Card Spades 13 ->
            "🂮"

        Card Hearts 1 ->
            "🂱"

        Card Hearts 2 ->
            "🂲"

        Card Hearts 3 ->
            "🂳"

        Card Hearts 4 ->
            "🂴"

        Card Hearts 5 ->
            "🂵"

        Card Hearts 6 ->
            "🂶"

        Card Hearts 7 ->
            "🂷"

        Card Hearts 8 ->
            "🂸"

        Card Hearts 9 ->
            "🂹"

        Card Hearts 10 ->
            "🂺"

        Card Hearts 11 ->
            "🂻"

        Card Hearts 12 ->
            "🂽"

        Card Hearts 13 ->
            "🂾"

        Card Diamonds 1 ->
            "🃁"

        Card Diamonds 2 ->
            "🃂"

        Card Diamonds 3 ->
            "🃃"

        Card Diamonds 4 ->
            "🃄"

        Card Diamonds 5 ->
            "🃅"

        Card Diamonds 6 ->
            "🃆"

        Card Diamonds 7 ->
            "🃇"

        Card Diamonds 8 ->
            "🃈"

        Card Diamonds 9 ->
            "🃉"

        Card Diamonds 10 ->
            "🃊"

        Card Diamonds 11 ->
            "🃋"

        Card Diamonds 12 ->
            "🃍"

        Card Diamonds 13 ->
            "🃎"

        Card Clubs 1 ->
            "🃑"

        Card Clubs 2 ->
            "🃒"

        Card Clubs 3 ->
            "🃓"

        Card Clubs 4 ->
            "🃔"

        Card Clubs 5 ->
            "🃕"

        Card Clubs 6 ->
            "🃖"

        Card Clubs 7 ->
            "🃗"

        Card Clubs 8 ->
            "🃘"

        Card Clubs 9 ->
            "🃙"

        Card Clubs 10 ->
            "🃚"

        Card Clubs 11 ->
            "🃛"

        Card Clubs 12 ->
            "🃝"

        Card Clubs 13 ->
            "🃞"

        Card _ _ ->
            "🂠"
