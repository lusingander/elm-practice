module Main exposing (main)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Decode.Pipeline


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
    { name : String -- full_name
    , url : String -- html_url
    , updated : String -- updated_at
    }


repositoryDecoder : Json.Decode.Decoder Repository
repositoryDecoder =
    Json.Decode.succeed Repository
        |> Json.Decode.Pipeline.required "full_name" Json.Decode.string
        |> Json.Decode.Pipeline.required "html_url" Json.Decode.string
        |> Json.Decode.Pipeline.required "updated_at" Json.Decode.string


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
    div
        [ style "margin" "20px"
        ]
        [ viewHeader
        , viewContent model
        ]


viewHeader : Html Msg
viewHeader =
    div
        [ style "margin" "10px"
        ]
        [ text "Recently updated Elm repositories on GitHub"
        , button
            [ onClick ClickReloadButton
            ]
            [ text "Reload"
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    case .status model of
        Loading ->
            div
                []
                [ text "Loading..." ]

        Failure ->
            div
                []
                [ text "Failed to load" ]

        Success repos ->
            div
                []
                (List.map viewRepository repos)


viewRepository : Repository -> Html Msg
viewRepository repo =
    div
        [ style "margin" "5px"
        ]
        [ span [] [ text (.updated repo) ]
        , span [] [ text (.name repo) ]
        , span [] [ text (.url repo) ]
        ]
