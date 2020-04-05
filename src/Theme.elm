module Theme exposing
    ( navBarColor
    , primaryColor
    , shadowColor
    , titleColor
    , topBarHeight
    , whiteColor
    )

import Css


topBarHeight : Float
topBarHeight =
    42


primaryColor : Css.Color
primaryColor =
    Css.rgb 16 152 247


shadowColor : Css.Color
shadowColor =
    Css.hex "#888"


whiteColor : Css.Color
whiteColor =
    Css.rgb 255 255 255


navBarColor : Css.Color
navBarColor =
    Css.rgb 235 235 235


titleColor : Css.Color
titleColor =
    Css.rgb 0 0 255
