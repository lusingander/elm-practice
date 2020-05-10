module Main exposing (main)

import Playground exposing (..)


main =
    game view update initMemory


type alias Memory =
    { bar : Bar
    }


type alias Bar =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


fieldWidth : Float
fieldWidth =
    800


fieldHeight : Float
fieldHeight =
    600


barWidth : Float
barWidth =
    fieldWidth / 10


barHeight : Float
barHeight =
    10


initMemory : Memory
initMemory =
    { bar =
        { x = 0
        , y = (barHeight * 2) - (fieldHeight / 2)
        , w = barWidth
        , h = barHeight
        }
    }


moveBar : Float -> Bar -> Bar
moveBar d bar =
    let
        newBar =
            { bar | x = .x bar + d }
    in
    if barIsInField newBar then
        newBar

    else
        bar


barLeft : Bar -> Float
barLeft bar =
    .x bar - (barWidth / 2)


barRight : Bar -> Float
barRight bar =
    .x bar + (barWidth / 2)


barIsInField : Bar -> Bool
barIsInField bar =
    -(fieldWidth / 2) <= barLeft bar && barRight bar <= (fieldWidth / 2)


view : Computer -> Memory -> List Shape
view computer memory =
    let
        bar =
            .bar memory
    in
    [ rectangle darkGray fieldWidth fieldHeight
    , showBar bar
        |> move (.x bar) (.y bar)
    ]


showBar : Bar -> Shape
showBar bar =
    rectangle blue (.w bar) (.h bar)


update : Computer -> Memory -> Memory
update computer memory =
    { memory
        | bar = moveBar (5 * toX computer.keyboard) (.bar memory)
    }
