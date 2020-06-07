module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Http


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
        , expect = Http.expectString GotText
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
    | Success String


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok fullText) ->
            ( { model
                | status = Success fullText
              }
            , Cmd.none
            )

        GotText (Err _) ->
            ( { model
                | status = Failure
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case .status model of
        Loading ->
            div
                []
                [ text "Loading..." ]

        Failure ->
            div
                []
                [ text "Failed to load" ]

        Success t ->
            div
                []
                [ text t ]
