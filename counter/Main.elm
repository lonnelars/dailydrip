port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


port jsMsgs : (Int -> msg) -> Sub msg


port increment : () -> Cmd msg


port storage : Int -> Cmd msg


port storageInput : (Int -> msg) -> Sub msg


type alias Model =
    { count : Int
    , increment : Int
    , decrement : Int
    }


type Msg
    = Increment
    | Decrement
    | NoOp
    | Set Int


initialModel : Model
initialModel =
    { count = 0
    , increment = 0
    , decrement = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            let
                newModel =
                    { model
                        | count = model.count + 1
                        , increment = model.increment + 1
                    }
            in
                ( newModel
                , Cmd.batch
                    [ increment ()
                    , storage newModel.count
                    ]
                )

        Decrement ->
            let
                newModel =
                    { model
                        | count = model.count - 1
                        , decrement = model.decrement + 1
                    }
            in
                ( newModel
                , storage newModel.count
                )

        NoOp ->
            ( model, Cmd.none )

        Set newCount ->
            ( { model | count = newCount }, Cmd.none )


mapJsMsg : Int -> Msg
mapJsMsg int =
    case int of
        1 ->
            Increment

        _ ->
            NoOp


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        , h3 [] [ text ("- clicked " ++ (toString model.decrement) ++ " times") ]
        , h3 [] [ text ("+ clicked " ++ (toString model.increment) ++ " times") ]
        ]


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch
        [ jsMsgs mapJsMsg
        , storageInput Set
        ]
