module Main exposing (main)

import Playground exposing (..)


main =
    game view update initMemory


type alias Memory =
    { playingState : PlayingState
    , bar : Bar
    , ball : Ball
    , blocks : List Block
    }


type PlayingState
    = Playing
    | GameOver


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


type alias Block =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


type BallHit
    = HitSideEdge
    | HitTopEdge
    | HitBlock
    | HitBar


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


blockMargin : Float
blockMargin =
    5


blockWidth : Float
blockWidth =
    70


blockHeight : Float
blockHeight =
    20


initMemory : Memory
initMemory =
    Memory Playing initBar initBall initBlocks


initBar : Bar
initBar =
    Bar 0 ((barHeight * 2) - (fieldHeight / 2)) barWidth barHeight


initBall : Ball
initBall =
    Ball 0 0 DownRight


initBlocks : List Block
initBlocks =
    createBlocks 6 0 (-(fieldWidth / 2) + blockMargin) ((fieldHeight / 2) - blockMargin)


createBlocks : Int -> Int -> Float -> Float -> List Block
createBlocks max lines leftTopX leftTopY =
    if lines == max then
        []

    else
        let
            newBlock =
                { x = leftTopX + blockWidth / 2
                , y = leftTopY - blockHeight / 2
                , w = blockWidth
                , h = blockHeight
                }
        in
        if leftTopX + blockWidth >= (fieldWidth / 2) then
            createBlocks max (lines + 1) (-(fieldWidth / 2) + blockMargin) (((fieldHeight / 2) - blockMargin) - ((blockHeight + blockMargin) * (lines + 1 |> toFloat)))

        else
            newBlock :: createBlocks max lines (leftTopX + blockWidth + (blockMargin * 2)) leftTopY


barLeft : Bar -> Float
barLeft bar =
    .x bar - (barWidth / 2)


barRight : Bar -> Float
barRight bar =
    .x bar + (barWidth / 2)


barTop : Bar -> Float
barTop bar =
    .y bar + (barHeight / 2)


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


blockLeft : Block -> Float
blockLeft block =
    .x block - (.w block / 2)


blockRight : Block -> Float
blockRight block =
    .x block + (.w block / 2)


blockTop : Block -> Float
blockTop block =
    .y block + (.h block / 2)


blockBottom : Block -> Float
blockBottom block =
    .y block - (.h block / 2)


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


ballHitBar : Ball -> Bar -> Bool
ballHitBar ball bar =
    barLeft bar <= .x ball && .x ball <= barRight bar && ballBottom ball <= barTop bar


ballHitBlock : Ball -> Block -> Bool
ballHitBlock ball block =
    let
        cx =
            .x ball

        cy =
            .y ball

        cr =
            ballRadius

        t =
            blockTop block

        b =
            blockBottom block

        l =
            blockLeft block

        r =
            blockRight block

        lcx2 =
            (l - cx) ^ 2

        rcx2 =
            (r - cx) ^ 2

        tcy2 =
            (t - cy) ^ 2

        bcy2 =
            (b - cy) ^ 2

        cr2 =
            cr ^ 2
    in
    (l <= cx && cx <= r && b - cr <= cy && cy <= t + cr)
        || (l - cr <= cx && cx <= r + cr && b <= cy && cy <= t)
        || (lcx2 + tcy2 <= cr2)
        || (rcx2 + tcy2 <= cr2)
        || (lcx2 + bcy2 <= cr2)
        || (rcx2 + bcy2 <= cr2)


breakBlock : Ball -> List Block -> List Block
breakBlock ball blocks =
    case blocks of
        [] ->
            []

        b :: bs ->
            if ballHitBlock ball b then
                bs

            else
                b :: breakBlock ball bs


view : Computer -> Memory -> List Shape
view _ memory =
    let
        bar =
            .bar memory

        ball =
            .ball memory

        blocks =
            .blocks memory
    in
    List.concat
        [ [ rectangle darkGray fieldWidth fieldHeight
          , showBar bar
          , showBall ball
          ]
        , showBlocks blocks
        , showGameMessage memory
        ]


showGameMessage : Memory -> List Shape
showGameMessage memory =
    case .playingState memory of
        GameOver ->
            [ words black "GAME OVER" |> scale 2 |> move 0 30
            , words black "Press SPACE to restart" |> move 0 -30
            ]

        _ ->
            []


showBar : Bar -> Shape
showBar bar =
    rectangle blue (.w bar) (.h bar)
        |> move (.x bar) (.y bar)


showBall : Ball -> Shape
showBall ball =
    circle darkRed ballRadius
        |> move (.x ball) (.y ball)


showBlocks : List Block -> List Shape
showBlocks blocks =
    List.map showBlock blocks


showBlock : Block -> Shape
showBlock block =
    rectangle darkYellow (.w block) (.h block)
        |> move (.x block) (.y block)


update : Computer -> Memory -> Memory
update computer memory =
    case .playingState memory of
        Playing ->
            let
                newBall =
                    moveBall memory

                newBlocks =
                    .blocks memory |> breakBlock newBall

                brokenBlock =
                    List.length (.blocks memory) /= List.length newBlocks
            in
            { memory
                | playingState = getPlayingState newBall
                , bar = .bar memory |> moveBar (5 * toX computer.keyboard)
                , ball =
                    if brokenBlock then
                        updateBallDirection HitBlock newBall

                    else
                        newBall
                , blocks = newBlocks
            }

        GameOver ->
            if computer.keyboard.space then
                initMemory

            else
                memory


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


moveBall : Memory -> Ball
moveBall memory =
    let
        bar =
            .bar memory

        ball =
            .ball memory

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
                updateBallDirection HitSideEdge newBall

            else if ballHitFieldTopEdge newBall then
                updateBallDirection HitTopEdge newBall

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
                updateBallDirection HitSideEdge newBall

            else if ballHitFieldTopEdge newBall then
                updateBallDirection HitTopEdge newBall

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
            if ballHitBar newBall bar then
                updateBallDirection HitBar newBall

            else if ballHitFieldLeftEdge newBall then
                updateBallDirection HitSideEdge newBall

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
            if ballHitBar newBall bar then
                updateBallDirection HitBar newBall

            else if ballHitFieldRightEdge newBall then
                updateBallDirection HitSideEdge newBall

            else
                newBall


updateBallDirection : BallHit -> Ball -> Ball
updateBallDirection hit ball =
    case hit of
        HitSideEdge ->
            case .dir ball of
                UpLeft ->
                    { ball | dir = UpRight }

                UpRight ->
                    { ball | dir = UpLeft }

                DownLeft ->
                    { ball | dir = DownRight }

                DownRight ->
                    { ball | dir = DownLeft }

        HitTopEdge ->
            case .dir ball of
                UpLeft ->
                    { ball | dir = DownLeft }

                UpRight ->
                    { ball | dir = DownRight }

                _ ->
                    ball

        HitBar ->
            case .dir ball of
                DownLeft ->
                    { ball | dir = UpLeft }

                DownRight ->
                    { ball | dir = UpRight }

                _ ->
                    ball

        HitBlock ->
            case .dir ball of
                UpLeft ->
                    { ball | dir = DownLeft }

                UpRight ->
                    { ball | dir = DownRight }

                DownLeft ->
                    { ball | dir = UpLeft }

                DownRight ->
                    { ball | dir = UpRight }


getPlayingState : Ball -> PlayingState
getPlayingState ball =
    if ballHitFieldBottomEdge ball then
        GameOver

    else
        Playing
