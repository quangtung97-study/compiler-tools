module Main exposing (main)

import Browser
import Css exposing (Style)
import Css.Transitions as T
import DFA
import Html as H0
import Html.Styled as H exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Icons.Hamburger as NavBarIcon
import NavBar
import Theme


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { openNavBar : Bool
    , navBar : NavBar.Model
    , dfa : DFA.Model
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { openNavBar = True
      , navBar = NavBar.init
      , dfa = DFA.init
      }
    , Cmd.none
    )


type Msg
    = ToggleNavBar
    | NavBarMsg NavBar.Msg
    | DFAMsg DFA.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleNavBar ->
            ( { model | openNavBar = not model.openNavBar }, Cmd.none )

        NavBarMsg navBarMsg ->
            ( { model
                | navBar = NavBar.update navBarMsg model.navBar
              }
            , Cmd.none
            )

        DFAMsg dfaMsg ->
            ( { model | dfa = DFA.update dfaMsg model.dfa }
            , Cmd.none
            )


topBarStyle : Style
topBarStyle =
    Css.batch
        [ Css.color (Css.rgb 255 0 0)
        , Css.backgroundColor Theme.primaryColor
        , Css.height (Css.px Theme.topBarHeight)
        , Css.displayFlex
        ]


topBarView : Html Msg
topBarView =
    div [ css [ topBarStyle ] ]
        [ div
            [ css
                [ Css.padding (Css.px 3)
                , Css.cursor Css.pointer
                ]
            , onClick ToggleNavBar
            ]
            [ NavBarIcon.view (Theme.topBarHeight - 6)
                [ Css.hover
                    [ Css.boxShadow5 (Css.px 0) (Css.px 0) (Css.px 3) (Css.px 3) Theme.shadowColor
                    ]
                ]
            ]
        ]


mainView : Model -> Html Msg
mainView model =
    let
        ( num, navBar ) =
            if model.openNavBar then
                ( 3, NavBar.view model.navBar )

            else
                ( 0, text "" )
    in
    div
        [ css
            [ Css.displayFlex
            , Css.height (Css.vh 100)
            ]
        ]
        [ div
            [ css
                [ Css.flex (Css.num num)
                , Css.backgroundColor Theme.navBarColor
                , T.transition [ T.flex 200 ]
                ]
            ]
            [ H.map NavBarMsg navBar ]
        , div
            [ css
                [ Css.flex (Css.num (16 - num))
                ]
            ]
            [ topBarView
            , div []
                [ H.map DFAMsg (DFA.view model.dfa)
                ]
            ]
        ]


bodyView : Model -> H0.Html Msg
bodyView model =
    toUnstyled (mainView model)


view : Model -> Browser.Document Msg
view model =
    { title = "Compiler"
    , body = [ bodyView model ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
