module Icons.Sigma exposing (view)

import Html.Styled exposing (Html)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as A


pathD : String
pathD =
    """
M22,0l0.1,6.3c-0.2,0-0.4,0-0.7,0c-0.3,0-0.4-0.1-0.5-0.4c0-0.2-0.1-0.3-0.1-0.5C20.2,
3.2,19,2,16.8,2 c-3,0-7.5,0-7.6,0S8.9,2,8.9,2.2c2.4,3.2,4.8,6.4,7.3,9.6C13.3,
15.2,7.5,22,7.5,22s7,0,10.7,0c2,0,3.4-0.6,4.1-2.4 c0.1-0.2,0.2-0.3,0.2-0.5C22.8,
17.8,24,18,24,18l-1.5,8c0,0-20.2,0-20.5,0s-0.5-0.2-0.5-0.3c3.6-4.3,
7-8.4,10.6-12.7 C8.5,8.6,1.5,0,1.5,0H22z
"""


view : Float -> Html msg
view w =
    let
        width =
            String.fromFloat w ++ "px"
    in
    svg
        [ A.viewBox "0 0 26 26"
        , A.width width
        , A.enableBackground "new 0 0 26 26"
        ]
        [ path
            [ A.d pathD ]
            []
        ]
