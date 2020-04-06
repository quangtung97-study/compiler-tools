module DFA exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Css
import DFA.Algorithm as Algorithm
import Html.Styled exposing (Html, div, input, span, sub, text)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onInput)
import Icons.Sigma as Sigma
import TextField


type alias Model =
    { alphabetText : String
    , stateText : String
    , dfa : Algorithm.DFA
    , valid : Bool
    }


init : Model
init =
    { alphabetText = ""
    , stateText = ""
    , dfa = Algorithm.init
    , valid = True
    }


type Msg
    = AlphabetTextChanged String
    | StateTextChanged String


update : Msg -> Model -> Model
update msg model =
    let
        dfa =
            model.dfa
    in
    case msg of
        AlphabetTextChanged s ->
            { model
                | alphabetText = s
                , dfa = Algorithm.alphabet dfa s
            }

        StateTextChanged s ->
            let
                newStates =
                    Algorithm.parseState s
            in
            { model
                | stateText = s
                , dfa =
                    { dfa
                        | states = Maybe.withDefault dfa.states newStates
                    }
                , valid =
                    case newStates of
                        Just _ ->
                            True

                        Nothing ->
                            False
            }


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
        , sub [] [ text subscripts ]
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
            , alphabetView model.dfa.alphabet
            ]
        , div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                ]
            ]
            [ stateInputView model.valid model.stateText
            , stateListView model.dfa.states
            ]
        ]
