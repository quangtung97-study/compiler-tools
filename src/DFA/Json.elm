module DFA.Json exposing
    ( encode
    , encodeToString
    )

import Array exposing (Array)
import DFA.Algorithm exposing (DFA, State)
import DFA.Transition as T
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
