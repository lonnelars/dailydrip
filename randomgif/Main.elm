module Main exposing (..)

import RandomGifPair exposing (init, update, view)
import Html as Html


main =
    Html.program
        { init = init "funny cats" "funny dogs"
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.none
