module Main exposing (main)

import Playground exposing (..)


main =
    game view update initMemory


type alias Memory =
    { bar : Bar
    , ball : Ball
    }


type alias Bar =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


type alias Ball =
    { x : Float
    , y : Float
    , dir : BallDirection
    }


type BallDirection
    = UpLeft
    | UpRight
    | DownLeft
    | DownRight


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


ballRadius : Float
ballRadius =
    10


initMemory : Memory
initMemory =
    { bar =
        { x = 0
        , y = (barHeight * 2) - (fieldHeight / 2)
        , w = barWidth
        , h = barHeight
        }
    , ball =
        { x = 0
        , y = 0
        , dir = DownRight
        }
    }


barLeft : Bar -> Float
barLeft bar =
    .x bar - (barWidth / 2)


barRight : Bar -> Float
barRight bar =
    .x bar + (barWidth / 2)


barIsInField : Bar -> Bool
barIsInField bar =
    let
        w =
            fieldWidth / 2
    in
    -w <= barLeft bar && barRight bar <= w


ballLeft : Ball -> Float
ballLeft ball =
    .x ball - ballRadius


ballRight : Ball -> Float
ballRight ball =
    .x ball + ballRadius


ballTop : Ball -> Float
ballTop ball =
    .y ball + ballRadius


ballBottom : Ball -> Float
ballBottom ball =
    .y ball - ballRadius


ballHitFieldLeftEdge : Ball -> Bool
ballHitFieldLeftEdge ball =
    ballLeft ball <= -(fieldWidth / 2)


ballHitFieldRightEdge : Ball -> Bool
ballHitFieldRightEdge ball =
    (fieldWidth / 2) <= ballRight ball


ballHitFieldTopEdge : Ball -> Bool
ballHitFieldTopEdge ball =
    (fieldHeight / 2) <= ballTop ball


ballHitFieldBottomEdge : Ball -> Bool
ballHitFieldBottomEdge ball =
    ballBottom ball <= -(fieldHeight / 2)


view : Computer -> Memory -> List Shape
view computer memory =
    let
        bar =
            .bar memory

        ball =
            .ball memory
    in
    [ rectangle darkGray fieldWidth fieldHeight
    , showBar bar
    , showBall ball
    ]


showBar : Bar -> Shape
showBar bar =
    rectangle blue (.w bar) (.h bar)
        |> move (.x bar) (.y bar)


showBall : Ball -> Shape
showBall ball =
    circle darkRed ballRadius
        |> move (.x ball) (.y ball)


update : Computer -> Memory -> Memory
update computer memory =
    { memory
        | bar = .bar memory |> moveBar (5 * toX computer.keyboard)
        , ball = .ball memory |> moveBall
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


moveBallDelta : Float
moveBallDelta =
    3


moveBall : Ball -> Ball
moveBall ball =
    let
        oldX =
            .x ball

        oldY =
            .y ball

        d =
            moveBallDelta
    in
    case .dir ball of
        UpLeft ->
            let
                newBall =
                    { ball
                        | x = oldX - d
                        , y = oldY + d
                    }
            in
            if ballHitFieldLeftEdge newBall then
                { newBall | dir = UpRight }

            else if ballHitFieldTopEdge newBall then
                { newBall | dir = DownLeft }

            else
                newBall

        UpRight ->
            let
                newBall =
                    { ball
                        | x = oldX + d
                        , y = oldY + d
                    }
            in
            if ballHitFieldRightEdge newBall then
                { newBall | dir = UpLeft }

            else if ballHitFieldTopEdge newBall then
                { newBall | dir = DownRight }

            else
                newBall

        DownLeft ->
            let
                newBall =
                    { ball
                        | x = oldX - d
                        , y = oldY - d
                    }
            in
            if ballHitFieldLeftEdge newBall then
                { newBall | dir = DownRight }

            else if ballHitFieldBottomEdge newBall then
                { newBall | dir = UpLeft }

            else
                newBall

        DownRight ->
            let
                newBall =
                    { ball
                        | x = oldX + d
                        , y = oldY - d
                    }
            in
            if ballHitFieldRightEdge newBall then
                { newBall | dir = DownLeft }

            else if ballHitFieldBottomEdge newBall then
                { newBall | dir = UpRight }

            else
                newBall
