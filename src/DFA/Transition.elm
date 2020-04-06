module DFA.Transition exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Array
import Css exposing (Style)
import DFA.Algorithm as Algorithm exposing (DFA)
import Dict exposing (Dict)
import Html.Styled exposing (Html, input, span, sub, table, td, text, th, tr)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onInput)


type alias EntryState =
    { state : Algorithm.State
    , valid : Bool
    }


type alias StateDict =
    Dict Int (Dict Int EntryState)


type alias Model =
    { inputTexts : Dict Int (Dict Int String)
    , stateDict : StateDict
    }


emptyEntryState : EntryState
emptyEntryState =
    { state = Algorithm.initState
    , valid = True
    }


init : Model
init =
    { inputTexts = Dict.empty
    , stateDict = Dict.empty
    }


getInputText : Int -> Int -> Model -> String
getInputText i j model =
    case Dict.get i model.inputTexts of
        Just subDict ->
            case Dict.get j subDict of
                Just s ->
                    s

                Nothing ->
                    ""

        Nothing ->
            ""


stringToEntryState : String -> EntryState -> EntryState
stringToEntryState s entry =
    let
        state =
            entry.state

        maybeState =
            Algorithm.parseState s
                |> Maybe.andThen
                    (\array ->
                        if Array.length array == 1 then
                            Just array

                        else
                            Nothing
                    )
                |> Maybe.andThen
                    (\array ->
                        Array.get 0 array
                    )

        newState =
            Maybe.withDefault state maybeState

        valid =
            case maybeState of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    { state = newState, valid = valid }


setStateDict : Int -> Int -> String -> StateDict -> StateDict
setStateDict i j s dict =
    let
        subDict =
            Dict.get i dict
                |> Maybe.withDefault Dict.empty

        entry =
            Dict.get j subDict
                |> Maybe.withDefault emptyEntryState

        newSubDict =
            Dict.insert j (stringToEntryState s entry) subDict
    in
    Dict.insert i newSubDict dict


stateDictValid : Int -> Int -> StateDict -> Bool
stateDictValid i j dict =
    Dict.get i dict
        |> Maybe.andThen
            (\subDict ->
                Dict.get j subDict
            )
        |> Maybe.map (\entry -> entry.valid)
        |> Maybe.withDefault True


setInputText : Int -> Int -> String -> Model -> Model
setInputText i j s model =
    let
        subDict =
            Dict.get i model.inputTexts
                |> Maybe.withDefault Dict.empty

        newSubDict =
            Dict.insert j s subDict
    in
    { model
        | inputTexts =
            model.inputTexts
                |> Dict.insert i newSubDict
        , stateDict = setStateDict i j s model.stateDict
    }


type Msg
    = InputTextChanged Int Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputTextChanged i j s ->
            setInputText i j s model


inputStyle : Style
inputStyle =
    Css.batch
        [ Css.width (Css.px 80)
        , Css.padding (Css.px 4)
        , Css.fontSize (Css.px 16)
        , Css.outline Css.none
        ]


inputValidStyle : Bool -> Style
inputValidStyle valid =
    Css.batch
        (if valid then
            []

         else
            [ Css.boxShadow5 Css.zero Css.zero (Css.px 8) Css.zero (Css.rgb 255 0 0)
            , Css.borderColor (Css.rgb 255 0 0)
            , Css.borderWidth (Css.px 1)
            ]
        )


headerColor : Css.Color
headerColor =
    Css.rgb 245 245 245


tableStyle : Style
tableStyle =
    Css.batch
        [ Css.backgroundColor (Css.rgb 200 200 200)
        , Css.boxShadow5 Css.zero Css.zero (Css.px 4) Css.zero (Css.rgb 100 100 100)
        , Css.margin2 (Css.px 12) Css.zero
        ]


headerStyle : Style
headerStyle =
    Css.batch
        [ Css.backgroundColor headerColor
        , Css.fontSize (Css.px 16)
        , Css.padding (Css.px 4)
        ]


firstColStyle : Style
firstColStyle =
    Css.batch
        [ Css.fontSize (Css.px 16)
        , Css.fontWeight Css.bold
        , Css.textAlign Css.center
        , Css.backgroundColor headerColor
        , Css.padding2 Css.zero (Css.px 8)
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


view : DFA -> Model -> Html Msg
view dfa model =
    let
        headers =
            dfa.alphabet
                |> Array.toList
                |> List.map
                    (\c ->
                        th [ css [ headerStyle ] ] [ text (String.fromChar c) ]
                    )

        len =
            Array.length dfa.alphabet

        inputs i =
            List.range 0 (len - 1)
                |> List.map
                    (\j ->
                        td []
                            [ input
                                [ value (getInputText i j model)
                                , css
                                    [ inputStyle
                                    , inputValidStyle (stateDictValid i j model.stateDict)
                                    ]
                                , onInput (InputTextChanged i j)
                                ]
                                []
                            ]
                    )

        rows =
            dfa.states
                |> Array.toList
                |> List.indexedMap
                    (\index s ->
                        tr []
                            (td [ css [ firstColStyle ] ] [ stateView s ]
                                :: inputs index
                            )
                    )
    in
    table [ css [ tableStyle ] ]
        (tr []
            (th [ css [ headerStyle ] ] []
                :: headers
            )
            :: rows
        )
