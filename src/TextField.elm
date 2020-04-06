module TextField exposing (build, init, withValid)

import Css exposing (Style)
import Html.Styled exposing (Attribute, Html, input)
import Html.Styled.Attributes exposing (css, type_)


type alias TextField =
    Bool


init : TextField
init =
    True


withValid : Bool -> TextField -> TextField
withValid valid textField =
    valid


style : Style
style =
    Css.batch
        [ Css.borderStyle Css.solid
        , Css.padding2 (Css.px 6) (Css.px 8)
        , Css.borderColor normalColor
        , Css.borderWidth (Css.px 1)
        , Css.borderRadius (Css.px 8)
        , Css.outline Css.none
        , Css.fontSize (Css.px 16)
        , Css.color (Css.hex "#333")
        ]


normalColor : Css.Color
normalColor =
    Css.rgb 180 180 180


invalidColor : Css.Color
invalidColor =
    Css.rgb 255 100 100


validStyle : Bool -> Style
validStyle valid =
    Css.focus
        (if valid then
            [ Css.boxShadow5 (Css.px 0) (Css.px 0) (Css.px 10) (Css.px 1) normalColor
            ]

         else
            [ Css.boxShadow5 (Css.px 0) (Css.px 0) (Css.px 10) (Css.px 1) invalidColor
            , Css.borderColor invalidColor
            ]
        )


build : List (Attribute msg) -> TextField -> Html msg
build attrs textField =
    input
        ([ type_ "text"
         , css [ style, validStyle textField ]
         ]
            ++ attrs
        )
        []
