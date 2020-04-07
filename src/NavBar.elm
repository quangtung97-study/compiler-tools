module NavBar exposing (Model, Msg, init, update, view)

import Css exposing (Style)
import Css.Transitions as T
import Divider
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Theme


type alias Model =
    { openScanner : Bool
    , openParser : Bool
    }


init : Model
init =
    { openScanner = False
    , openParser = False
    }


type Msg
    = ToggleScanner
    | ToggleParser


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleScanner ->
            { model | openScanner = not model.openScanner }

        ToggleParser ->
            { model | openParser = not model.openParser }


navItemColor : Css.Color
navItemColor =
    Css.rgb 2 88 148


logoView : Html Msg
logoView =
    div
        [ css
            [ Css.fontSize (Css.px 30)
            , Css.fontWeight Css.bold
            , Css.color Theme.titleColor
            , Css.height (Css.px 64)
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.spaceAround
            , Css.backgroundColor Theme.whiteColor
            ]
        ]
        [ div
            []
            [ text "Compiler" ]
        ]


subItemStyle : Bool -> Int -> Style
subItemStyle open count =
    if open then
        Css.batch
            [ Css.maxHeight (Css.px (36.0 * toFloat count))
            , T.transition
                [ T.maxHeight3 200 0 T.easeInOut
                ]
            , Css.overflow Css.hidden
            ]

    else
        Css.batch
            [ Css.maxHeight Css.zero
            , T.transition
                [ T.maxHeight3 200 0 T.easeInOut
                ]
            , Css.overflow Css.hidden
            ]


itemView : Bool -> Msg -> String -> List String -> Html Msg
itemView open clickMsg title subItems =
    let
        subItemView itemTitle =
            div
                [ css
                    [ Css.padding (Css.px 8)
                    , Css.lineHeight (Css.px 20)
                    , Css.fontSize (Css.px 16)
                    , Css.fontWeight Css.bold
                    , Css.paddingLeft (Css.px 20)
                    , Css.color navItemColor
                    , Css.cursor Css.pointer
                    , Css.hover
                        [ Css.backgroundColor (Css.rgb 212 212 212)
                        ]
                    ]
                ]
                [ text itemTitle ]

        subItemViews =
            div
                [ css
                    [ subItemStyle open (List.length subItems)
                    ]
                ]
                (List.map
                    subItemView
                    subItems
                )
    in
    div
        []
        [ div
            [ css
                [ Css.padding2 (Css.px 12) (Css.px 12)
                , Css.fontSize (Css.px 20)
                , Css.fontWeight Css.bold
                , Css.color navItemColor
                , Css.cursor Css.pointer
                , Css.hover
                    [ Css.backgroundColor (Css.rgb 212 212 212)
                    ]
                ]
            , onClick clickMsg
            ]
            [ text title
            ]
        , subItemViews
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ logoView
        , Divider.view
        , itemView model.openScanner ToggleScanner "Scanner" [ "DFA", "NFA" ]
        , itemView model.openParser ToggleParser "Parser" [ "Backtrack", "LL(k)", "LR(1)" ]
        ]
