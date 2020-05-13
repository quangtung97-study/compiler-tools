module Button exposing
    ( Color(..)
    , build
    , init
    , withCaption
    , withColor
    , withDisabled
    , withStyle
    )

import Css
import Html.Styled exposing (Html, button, text)
import Html.Styled.Attributes exposing (css, disabled)
import Html.Styled.Events exposing (onClick)


type Color
    = Primary
    | Secondary
    | Info


type alias Button =
    { caption : String
    , color : Color
    , style : Css.Style
    , disabled : Bool
    }


init : Button
init =
    { caption = ""
    , color = Primary
    , style = Css.batch []
    , disabled = False
    }


withCaption : String -> Button -> Button
withCaption s button =
    { button | caption = s }


withColor : Color -> Button -> Button
withColor color button =
    { button | color = color }


withStyle : Css.Style -> Button -> Button
withStyle style button =
    { button | style = style }


withDisabled : Bool -> Button -> Button
withDisabled b button =
    { button | disabled = b }


buttonColor : Button -> Css.Style
buttonColor button =
    if button.disabled then
        Css.batch
            [ Css.backgroundColor (Css.rgb 150 150 150)
            ]

    else
        case button.color of
            Primary ->
                Css.batch
                    [ Css.backgroundColor (Css.rgb 16 152 247)
                    , Css.hover
                        [ Css.backgroundColor (Css.rgb 25 180 255)
                        ]
                    ]

            Secondary ->
                Css.batch
                    [ Css.backgroundColor (Css.rgb 235 0 0)
                    , Css.hover
                        [ Css.backgroundColor (Css.rgb 255 0 0)
                        ]
                    ]

            Info ->
                Css.batch
                    [ Css.backgroundColor (Css.rgb 0 160 0)
                    , Css.hover
                        [ Css.backgroundColor (Css.rgb 0 180 0)
                        ]
                    ]


build : msg -> Button -> Html msg
build msg buttonData =
    button
        [ onClick msg
        , disabled buttonData.disabled
        , css
            [ Css.padding (Css.px 8)
            , buttonColor buttonData
            , Css.color (Css.rgb 255 255 255)
            , Css.fontWeight Css.bold
            , Css.borderStyle Css.none
            , Css.fontSize (Css.px 18)
            , buttonData.style
            ]
        ]
        [ text buttonData.caption ]
