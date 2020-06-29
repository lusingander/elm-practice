module Card exposing (Card, allCards, numberEquals, showCard, showCardBack)

import Maybe.Extra exposing (cons)


type Card
    = Card Suit Rank
    | Joker


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


numberToRank : Int -> Maybe Rank
numberToRank n =
    case n of
        1 ->
            Just Ace

        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        6 ->
            Just Six

        7 ->
            Just Seven

        8 ->
            Just Eight

        9 ->
            Just Nine

        10 ->
            Just Ten

        11 ->
            Just Jack

        12 ->
            Just Queen

        13 ->
            Just King

        _ ->
            Nothing


rankToNumber : Rank -> Int
rankToNumber r =
    case r of
        Ace ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13


cardNumber : Card -> Int
cardNumber c =
    case c of
        Card _ r ->
            rankToNumber r

        Joker ->
            0


allCards : List Card
allCards =
    List.concat
        [ suitCards Spades
        , suitCards Hearts
        , suitCards Diamonds
        , suitCards Clubs
        ]


suitCards : Suit -> List Card
suitCards s =
    List.range 1 13
        |> List.map numberToRank
        |> List.foldr cons []
        |> List.map (Card s)


numberEquals : Card -> Card -> Bool
numberEquals c1 c2 =
    cardNumber c1 == cardNumber c2


showCardBack : String
showCardBack =
    "🂠"


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
