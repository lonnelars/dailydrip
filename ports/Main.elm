port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)


port jsMsgs : (Int -> msg) -> Sub msg


port outbound : () -> Cmd msg


type alias Model =
    Int


type Msg
    = Increment
    | SendValue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        SendValue ->
            ( model, outbound () )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model) ]
        , button [ onClick SendValue ] [ text "send message" ]
        ]


subscriptions model =
    jsMsgs (always Increment)


main =
    Html.program
        { init = ( 0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
