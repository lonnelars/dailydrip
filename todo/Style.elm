module Style exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)


css =
    stylesheet
        [ html
            [ margin zero
            , padding zero
            ]
        , body
            [ margin zero
            , padding zero
            ]
        , button
            [ margin zero
            , padding zero
            , border zero
            , background none
            ]
        ]
