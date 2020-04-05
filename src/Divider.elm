module Divider exposing (view)

import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)


view : Html msg
view =
    div
        [ css
            [ Css.height (Css.px 2)
            , Css.backgroundColor (Css.rgb 160 160 160)
            ]
        ]
        []
