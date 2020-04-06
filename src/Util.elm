module Util exposing (onKeyDown)

import Html.Styled exposing (Attribute)
import Html.Styled.Events exposing (keyCode, on)
import Json.Decode as D


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (D.map tagger keyCode)
