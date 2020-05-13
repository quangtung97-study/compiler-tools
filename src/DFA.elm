module DFA exposing (Model, Msg, init, update, updateCmd, view)

import Array exposing (Array)
import Button
import Css exposing (Style)
import DFA.Algorithm as Algorithm
import DFA.Json
    exposing
        ( alphabetToText
        , decodeFromString
        , encodeToString
        , maybeStateToText
        , statesToText
        )
import DFA.Transition as Transition
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html.Styled as H exposing (Html, div, span, sub, text)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onInput)
import Icons.Sigma as Sigma
import Task
import TextField


type alias Model =
    { alphabetText : String
    , statesText : String
    , finalStatesText : String
    , startStateText : String
    , states : Array Algorithm.State
    , alphabet : Array Char
    , statesValid : Bool
    , transition : Transition.Model
    , finalStates : Array Algorithm.State
    , finalStatesValid : Bool
    , startState : Maybe Algorithm.State
    , startStateValid : Bool
    , loadFileError : String
    }


init : Model
init =
    { alphabetText = ""
    , statesText = ""
    , finalStatesText = ""
    , startStateText = ""
    , states = Array.empty
    , alphabet = Array.empty
    , statesValid = True
    , transition = Transition.init
    , finalStates = Array.empty
    , finalStatesValid = True
    , startState = Nothing
    , startStateValid = True
    , loadFileError = ""
    }


type Msg
    = AlphabetTextChanged String
    | StatesTextChanged String
    | FinalStatesTextChanged String
    | StartStateTextChanged String
    | TransitionMsg Transition.Msg
    | SaveToFileClicked
    | LoadFromFileClicked
    | FileSelected File
    | FileLoaded String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AlphabetTextChanged s ->
            { model | alphabetText = s }
                |> Algorithm.updateAlphabet s

        StatesTextChanged s ->
            { model | statesText = s }
                |> Algorithm.updateStates s

        FinalStatesTextChanged s ->
            { model | finalStatesText = s }
                |> Algorithm.updateFinalStates s

        StartStateTextChanged s ->
            { model | startStateText = s }
                |> Algorithm.updateStartState s

        TransitionMsg tMsg ->
            { model | transition = Transition.update tMsg model.transition }

        SaveToFileClicked ->
            model

        LoadFromFileClicked ->
            model

        FileSelected _ ->
            model

        FileLoaded s ->
            case decodeFromString init s of
                Err error ->
                    { model | loadFileError = error }

                Ok ( newModel, newTransition ) ->
                    { newModel
                        | alphabetText = alphabetToText newModel.alphabet
                        , statesText = statesToText newModel.states
                        , finalStatesText = statesToText newModel.finalStates
                        , startStateText = maybeStateToText newModel.startState
                        , transition = newTransition
                    }


updateCmd : Msg -> Model -> Cmd Msg
updateCmd msg model =
    case msg of
        AlphabetTextChanged _ ->
            Cmd.none

        StatesTextChanged _ ->
            Cmd.none

        FinalStatesTextChanged _ ->
            Cmd.none

        StartStateTextChanged _ ->
            Cmd.none

        TransitionMsg _ ->
            Cmd.none

        SaveToFileClicked ->
            encodeToString model model.transition
                |> Download.string "dfa.json" "application/json"

        LoadFromFileClicked ->
            Select.file [ "application/json" ] FileSelected

        FileSelected file ->
            Task.perform FileLoaded (File.toString file)

        FileLoaded _ ->
            Cmd.none


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


inputView : String -> (String -> Msg) -> Bool -> String -> Html Msg
inputView title onMsg valid t =
    div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
        [ div [ css [ Css.fontSize (Css.px 20), Css.fontWeight Css.bold ] ]
            [ text title ]
        , div
            [ css
                [ Css.margin (Css.px 4)
                ]
            ]
            [ TextField.init
                |> TextField.withValid valid
                |> TextField.build [ value t, onInput onMsg ]
            ]
        ]


stateInputView : Bool -> String -> Html Msg
stateInputView =
    inputView "States: " StatesTextChanged


finalStatesInputView : Bool -> String -> Html Msg
finalStatesInputView =
    inputView "Final States: " FinalStatesTextChanged


startStateInputView : Bool -> String -> Html Msg
startStateInputView =
    inputView "Start State: " StartStateTextChanged


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


finalStatesStyles : Bool -> List Style
finalStatesStyles valid =
    let
        styles =
            [ Css.displayFlex
            , Css.fontSize (Css.px 20)
            , Css.justifyContent Css.center
            ]
    in
    if valid then
        styles

    else
        Css.color (Css.rgb 255 0 0) :: styles


finalStatesListView : Array Algorithm.State -> Bool -> Html Msg
finalStatesListView states valid =
    div
        [ css (finalStatesStyles valid)
        ]
        [ div
            []
            (text "F = {"
                :: (states
                        |> Array.toList
                        |> List.map stateView
                        |> List.intersperse (text ", ")
                   )
                ++ [ text "}" ]
            )
        ]


startStateView : Maybe Algorithm.State -> Bool -> Html Msg
startStateView state valid =
    div
        [ css (finalStatesStyles valid)
        ]
        [ div
            []
            [ text "S = "
            , state
                |> Maybe.map stateView
                |> Maybe.withDefault (text "")
            ]
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
            [ stateInputView model.statesValid model.statesText
            , stateListView model.states
            ]
        , div []
            [ H.map TransitionMsg (Transition.view model model.transition)
            ]
        , div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                ]
            ]
            [ finalStatesInputView model.finalStatesValid model.finalStatesText
            , finalStatesListView model.finalStates (Algorithm.statesContainFinalStates model)
            ]
        , div
            [ css
                [ Css.displayFlex
                , Css.alignItems Css.center
                ]
            ]
            [ startStateInputView model.startStateValid model.startStateText
            , startStateView model.startState (Algorithm.statesContainStartState model)
            ]
        , div
            [ css
                [ Css.margin (Css.px 4)
                ]
            ]
            [ Button.init
                |> Button.withCaption "Load from file"
                |> Button.withColor Button.Primary
                |> Button.build LoadFromFileClicked
            , Button.init
                |> Button.withCaption "Save to file"
                |> Button.withColor Button.Primary
                |> Button.withStyle (Css.marginLeft (Css.px 8))
                |> Button.build SaveToFileClicked
            ]
        , div
            [ css
                [ Css.color (Css.rgb 255 0 0)
                ]
            ]
            [ text model.loadFileError ]
        ]
