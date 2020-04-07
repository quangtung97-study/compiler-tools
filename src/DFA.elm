module DFA exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Css exposing (Style)
import DFA.Algorithm as Algorithm
import DFA.Transition as Transition
import Html.Styled as H exposing (Html, div, span, sub, text)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onInput)
import Icons.Sigma as Sigma
import TextField


type alias Model =
    { alphabetText : String
    , stateText : String
    , states : Array Algorithm.State
    , alphabet : Array Char
    , statesValid : Bool
    , transition : Transition.Model
    }


init : Model
init =
    { alphabetText = ""
    , stateText = ""
    , states = Array.empty
    , alphabet = Array.empty
    , statesValid = True
    , transition = Transition.init
    }


type Msg
    = AlphabetTextChanged String
    | StateTextChanged String
    | TransitionMsg Transition.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        AlphabetTextChanged s ->
            { model | alphabetText = s }
                |> Algorithm.updateAlphabet s

        StateTextChanged s ->
            { model | stateText = s }
                |> Algorithm.updateStates s

        TransitionMsg tMsg ->
            { model | transition = Transition.update tMsg model.transition }


alphabetView : Array Char -> Html Msg
alphabetView alphabet =
    div
        [ css
            [ Css.displayFlex
            , Css.fontSize (Css.px 20)
            , Css.justifyContent Css.center
            ]
        ]
        [ Sigma.view 18
        , div
            []
            (text " ={"
                :: (alphabet
                        |> Array.toList
                        |> List.map (\c -> text (String.fromChar c))
                        |> List.intersperse (text ", ")
                   )
                ++ [ text "}" ]
            )
        ]


alphabetInputView : String -> Html Msg
alphabetInputView t =
    div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
        [ div [ css [ Css.fontSize (Css.px 20), Css.fontWeight Css.bold ] ]
            [ text "Alphabet: " ]
        , div
            [ css [ Css.margin (Css.px 4) ]
            ]
            [ TextField.init
                |> TextField.build [ value t, onInput AlphabetTextChanged ]
            ]
        ]


stateInputView : Bool -> String -> Html Msg
stateInputView valid t =
    div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
        [ div [ css [ Css.fontSize (Css.px 20), Css.fontWeight Css.bold ] ]
            [ text "States: " ]
        , div
            [ css
                [ Css.margin (Css.px 4)
                ]
            ]
            [ TextField.init
                |> TextField.withValid valid
                |> TextField.build [ value t, onInput StateTextChanged ]
            ]
        ]


subStyle : Style
subStyle =
    Css.batch
        [ Css.fontSize (Css.px 14)
        ]


stateView : Algorithm.State -> Html Msg
stateView state =
    let
        subscripts =
            state.subscripts
                |> Array.toList
                |> List.intersperse ", "
                |> String.concat
    in
    span []
        [ text state.name
        , sub [ css [ subStyle ] ] [ text subscripts ]
        ]


stateListView : Array Algorithm.State -> Html Msg
stateListView states =
    div
        [ css
            [ Css.displayFlex
            , Css.fontSize (Css.px 20)
            , Css.justifyContent Css.center
            ]
        ]
        [ div
            []
            (text "Q = {"
                :: (states
                        |> Array.toList
                        |> List.map stateView
                        |> List.intersperse (text ", ")
                   )
                ++ [ text "}" ]
            )
        ]


view : Model -> Html Msg
view model =
    div
        [ css [ Css.margin (Css.px 8) ]
        ]
        [ div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                ]
            ]
            [ alphabetInputView model.alphabetText
            , alphabetView model.alphabet
            ]
        , div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                ]
            ]
            [ stateInputView model.statesValid model.stateText
            , stateListView model.states
            ]
        , div []
            [ H.map TransitionMsg (Transition.view model model.transition)
            ]
        ]
