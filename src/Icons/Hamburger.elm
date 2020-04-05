module Icons.Hamburger exposing (view)

import Css exposing (Style)
import Html.Styled exposing (Html)
import Svg.Styled exposing (line, svg)
import Svg.Styled.Attributes as A


iconStyle : Style
iconStyle =
    Css.batch
        [ Css.backgroundColor (Css.rgb 255 255 255)
        ]


hline : String -> Html msg
hline y =
    line
        [ A.x1 "15"
        , A.y1 y
        , A.x2 "85"
        , A.y2 y
        , A.stroke "black"
        , A.strokeWidth "10"
        , A.strokeLinecap "round"
        ]
        []


view : Float -> List Style -> Html msg
view width styles =
    let
        widthStr =
            String.fromFloat width ++ "px"
    in
    svg
        [ A.width widthStr
        , A.height widthStr
        , A.viewBox "0 0 100 100"
        , A.css (iconStyle :: styles)
        ]
        [ hline "25"
        , hline "50"
        , hline "75"
        ]
