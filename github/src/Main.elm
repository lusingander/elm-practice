module Main exposing (main)

import Browser
import Html exposing (Html, div)



-- https://api.github.com/search/repositories?q=language:elm&sort=updated&order=desc


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( initModel
    , Cmd.none
    )


type alias Model =
    Int


initModel : Model
initModel =
    0


type Msg
    = Default


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Default ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        []
        []
