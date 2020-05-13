module DFA.Json exposing
    ( alphabetToText
    , decodeFromString
    , encode
    , encodeToString
    , maybeStateToText
    , statesToText
    )

import Array exposing (Array)
import DFA.Algorithm exposing (DFA, State)
import DFA.Transition as T
import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


encodeAlphabet : Array Char -> E.Value
encodeAlphabet alphabet =
    alphabet
        |> Array.toList
        |> List.map String.fromChar
        |> E.list E.string


encodeState : State -> E.Value
encodeState s =
    E.object
        [ ( "name", E.string s.name )
        , ( "subscripts", E.list E.string (Array.toList s.subscripts) )
        ]


encodeStates : Array State -> E.Value
encodeStates states =
    states
        |> Array.toList
        |> E.list encodeState


tupleToString : ( Int, Int ) -> String
tupleToString ( i, j ) =
    String.fromInt i ++ "," ++ String.fromInt j


stringToTuple : String -> Maybe ( Int, Int )
stringToTuple s =
    let
        numbers =
            String.split "," s
                |> List.filterMap String.toInt
    in
    case numbers of
        a :: b :: _ ->
            Just ( a, b )

        _ ->
            Nothing


encodeTransition : T.Model -> E.Value
encodeTransition model =
    model.stateDict
        |> E.dict tupleToString (\entry -> encodeState entry.state)


encode : DFA a -> T.Model -> E.Value
encode dfa transition =
    E.object
        [ ( "alphabet", encodeAlphabet dfa.alphabet )
        , ( "states", encodeStates dfa.states )
        , ( "finalStates", encodeStates dfa.finalStates )
        , ( "startState"
          , dfa.startState
                |> Maybe.map encodeState
                |> Maybe.withDefault E.null
          )
        , ( "transition", encodeTransition transition )
        ]


encodeToString : DFA a -> T.Model -> String
encodeToString dfa transition =
    E.encode 2 (encode dfa transition)


type alias DFAJson =
    { alphabet : List Char
    , states : List State
    , finalStates : List State
    , startState : State
    , transition : Dict ( Int, Int ) T.EntryState
    }


alphabetDecoder : D.Decoder (List Char)
alphabetDecoder =
    D.string
        |> D.map (\s -> List.head (String.toList s))
        |> D.list
        |> D.map (List.filterMap (\e -> e))


subscriptsDecoder : D.Decoder (Array String)
subscriptsDecoder =
    D.string
        |> D.list
        |> D.map Array.fromList


stateDecoder : D.Decoder State
stateDecoder =
    D.succeed State
        |> required "name" D.string
        |> required "subscripts" subscriptsDecoder


decodeStateDict : Dict String State -> Dict ( Int, Int ) T.EntryState
decodeStateDict dict =
    dict
        |> Dict.toList
        |> List.filterMap
            (\( key, state ) ->
                let
                    tuple =
                        stringToTuple key
                in
                tuple
                    |> Maybe.map (\t -> ( t, { valid = True, state = state } ))
            )
        |> Dict.fromList


stateDictDecoder : D.Decoder (Dict ( Int, Int ) T.EntryState)
stateDictDecoder =
    D.dict stateDecoder
        |> D.map decodeStateDict


decoder : D.Decoder DFAJson
decoder =
    D.succeed DFAJson
        |> required "alphabet" alphabetDecoder
        |> required "states" (D.list stateDecoder)
        |> required "finalStates" (D.list stateDecoder)
        |> required "startState" stateDecoder
        |> required "transition" stateDictDecoder


decodeFromString : DFA a -> String -> Result String ( DFA a, T.Model )
decodeFromString dfa s =
    D.decodeString decoder s
        |> Result.mapError D.errorToString
        |> Result.map
            (\r ->
                ( { dfa
                    | alphabet = Array.fromList r.alphabet
                    , states = Array.fromList r.states
                    , finalStates = Array.fromList r.finalStates
                    , startState = Just r.startState
                  }
                , { stateDict = r.transition
                  , inputTexts = stateDictToTexts r.transition
                  }
                )
            )


stateToText : State -> String
stateToText state =
    let
        suffix =
            if Array.length state.subscripts == 0 then
                ""

            else
                "_" ++ (state.subscripts |> Array.toList |> String.join ",")
    in
    state.name ++ suffix


statesToText : Array State -> String
statesToText states =
    states
        |> Array.toList
        |> List.map stateToText
        |> String.join " "


maybeStateToText : Maybe State -> String
maybeStateToText m =
    m
        |> Maybe.map stateToText
        |> Maybe.withDefault ""


alphabetToText : Array Char -> String
alphabetToText alphabet =
    alphabet
        |> Array.toList
        |> List.map (\c -> String.fromChar c)
        |> String.join ""


stateDictToTexts : Dict ( Int, Int ) T.EntryState -> Dict ( Int, Int ) String
stateDictToTexts dict =
    Dict.map (\_ v -> stateToText v.state) dict
