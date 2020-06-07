module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (href, rel, style, target)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


repositoriesUrl : String
repositoriesUrl =
    "https://api.github.com/search/repositories?q=language:elm&sort=updated&order=desc"


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Http.get
        { url = repositoriesUrl
        , expect = Http.expectJson GotRepositories repositoriesDecoder
        }
    )


type alias Model =
    { status : Status
    }


initModel : Model
initModel =
    { status = Loading
    }


type Status
    = Loading
    | Failure
    | Success Repositories


type alias Repositories =
    List Repository


repositoriesDecoder : Json.Decode.Decoder Repositories
repositoriesDecoder =
    Json.Decode.field "items" <|
        Json.Decode.list repositoryDecoder


type alias Repository =
    { name : String
    , url : String
    , updated : Time.Posix
    }


repositoryDecoder : Json.Decode.Decoder Repository
repositoryDecoder =
    Json.Decode.succeed Repository
        |> Json.Decode.Pipeline.required "full_name" Json.Decode.string
        |> Json.Decode.Pipeline.required "html_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "updated_at" Json.Decode.Extra.datetime


formatTime : Time.Posix -> String
formatTime t =
    let
        zone =
            Time.utc

        year =
            Time.toYear zone t |> String.fromInt

        month =
            Time.toMonth zone t |> monthToInt |> String.fromInt |> zeroPad2

        day =
            Time.toDay zone t |> String.fromInt |> zeroPad2

        hour =
            Time.toHour zone t |> String.fromInt |> zeroPad2

        minute =
            Time.toMinute zone t |> String.fromInt |> zeroPad2

        second =
            Time.toSecond zone t |> String.fromInt |> zeroPad2
    in
    String.join " "
        [ String.join "/" [ year, month, day ]
        , String.join ":" [ hour, minute, second ]
        ]


zeroPad2 : String -> String
zeroPad2 =
    String.padLeft 2 '0'


monthToInt : Time.Month -> Int
monthToInt m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


type Msg
    = GotRepositories (Result Http.Error Repositories)
    | ClickReloadButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRepositories (Ok repositories) ->
            ( { model
                | status = Success repositories
              }
            , Cmd.none
            )

        GotRepositories (Err _) ->
            ( { model
                | status = Failure
              }
            , Cmd.none
            )

        ClickReloadButton ->
            ( { model
                | status = Loading
              }
            , Http.get
                { url = repositoriesUrl
                , expect = Http.expectJson GotRepositories repositoriesDecoder
                }
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    Html.div
        [ style "margin" "20px"
        ]
        [ viewHeader
        , viewContent model
        ]


viewHeader : Html Msg
viewHeader =
    Html.div
        [ style "margin" "10px"
        ]
        [ Html.text "Recently updated Elm repositories on GitHub"
        , Html.button
            [ onClick ClickReloadButton
            ]
            [ Html.text "Reload"
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    case .status model of
        Loading ->
            Html.div
                []
                [ Html.text "Loading..." ]

        Failure ->
            Html.div
                []
                [ Html.text "Failed to load" ]

        Success repos ->
            Html.div
                []
                (List.map viewRepository repos)


viewRepository : Repository -> Html Msg
viewRepository repo =
    Html.div
        [ style "margin" "5px 0"
        ]
        [ Html.span
            [ style "margin" "0 5px"
            ]
            [ Html.text <| formatTime <| .updated repo
            ]
        , Html.span
            [ style "margin" "0 5px"
            ]
            [ Html.text (.name repo)
            ]
        , Html.a
            [ href (.url repo)
            , target "_blank"
            , rel "noopener noreferrer"
            , style "margin" "0 5px"
            ]
            [ Html.text (.url repo)
            ]
        ]
