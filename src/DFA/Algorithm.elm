module DFA.Algorithm exposing
    ( DFA
    , State
    , containsState
    , emptyState
    , parseStates
    , statesContainFinalStates
    , statesContainStartState
    , updateAlphabet
    , updateFinalStates
    , updateStartState
    , updateStates
    )

import Array exposing (Array)
import Set


type alias State =
    { name : String
    , subscripts : Array String
    }


emptyState : State
emptyState =
    { name = "", subscripts = Array.empty }


type alias DFA a =
    { a
        | states : Array State
        , alphabet : Array Char
        , statesValid : Bool
        , finalStates : Array State
        , finalStatesValid : Bool
        , startState : Maybe State
        , startStateValid : Bool
    }


fromAlphabetString : String -> Array Char
fromAlphabetString s =
    String.toList s
        |> List.filter (\c -> (c /= ' ') && (c /= '\t'))
        |> Set.fromList
        |> Set.toList
        |> Array.fromList


updateAlphabet : String -> DFA a -> DFA a
updateAlphabet s dfa =
    { dfa | alphabet = fromAlphabetString s }


containsState : State -> Array State -> Bool
containsState s states =
    states
        |> Array.toList
        |> List.any (\e -> e == s)


type ScanState
    = Init
    | Name
    | Underscore
    | Subscripts
    | Comma


stateNameAppend : State -> Char -> State
stateNameAppend state c =
    { state | name = String.append state.name (String.fromChar c) }


stateSubAppend : State -> Char -> State
stateSubAppend state c =
    let
        index =
            Array.length state.subscripts - 1

        string =
            Maybe.withDefault ""
                (Array.get index state.subscripts)
    in
    { state
        | subscripts =
            Array.set index
                (string ++ String.fromChar c)
                state.subscripts
    }


stateSubNew : State -> Char -> State
stateSubNew state c =
    { state | subscripts = Array.push (String.fromChar c) state.subscripts }


scan : State -> ScanState -> Char -> Maybe ( State, ScanState )
scan current q c =
    case q of
        Init ->
            if c == ' ' || c == '\t' then
                Just ( emptyState, Init )

            else if Char.isAlphaNum c then
                Just ( stateNameAppend current c, Name )

            else
                Nothing

        Name ->
            if c == ' ' || c == '\t' then
                Just ( emptyState, Init )

            else if Char.isAlphaNum c then
                Just ( stateNameAppend current c, Name )

            else if c == '_' then
                Just ( current, Underscore )

            else
                Nothing

        Underscore ->
            if Char.isAlphaNum c then
                Just ( stateSubNew current c, Subscripts )

            else
                Nothing

        Subscripts ->
            if c == ' ' || c == '\t' then
                Just ( emptyState, Init )

            else if Char.isAlphaNum c then
                Just ( stateSubAppend current c, Subscripts )

            else if c == ',' then
                Just ( current, Comma )

            else
                Nothing

        Comma ->
            if Char.isAlphaNum c then
                Just ( stateSubNew current c, Subscripts )

            else
                Nothing


scanRecur : State -> ScanState -> List State -> List Char -> Maybe ( State, ScanState, List State )
scanRecur current q stateList charList =
    case charList of
        c :: xs ->
            scan current q c
                |> Maybe.andThen
                    (\( newCurrent, newQ ) ->
                        case ( q, newQ ) of
                            ( Init, Init ) ->
                                scanRecur emptyState Init stateList xs

                            ( _, Init ) ->
                                scanRecur emptyState Init (current :: stateList) charList

                            ( _, _ ) ->
                                scanRecur newCurrent newQ stateList xs
                    )

        [] ->
            case q of
                Name ->
                    Just ( current, q, current :: stateList )

                Subscripts ->
                    Just ( current, q, current :: stateList )

                Init ->
                    Just ( current, q, stateList )

                _ ->
                    Nothing


parseStates : String -> Maybe (Array State)
parseStates s =
    String.toList s
        |> scanRecur emptyState Init []
        |> Maybe.map
            (\( _, _, qs ) ->
                Array.fromList (List.reverse qs)
            )


stateToComparable : State -> ( String, List String )
stateToComparable s =
    ( s.name, Array.toList s.subscripts )


comparableToState : ( String, List String ) -> State
comparableToState ( name, subscripts ) =
    { name = name, subscripts = Array.fromList subscripts }


parseStatesUnique : String -> Maybe (Array State)
parseStatesUnique s =
    parseStates s
        |> Maybe.map
            (\array ->
                array
                    |> Array.toList
                    |> List.map stateToComparable
                    |> Set.fromList
                    |> Set.toList
                    |> List.map comparableToState
                    |> Array.fromList
            )


updateStates : String -> DFA a -> DFA a
updateStates s dfa =
    let
        result =
            parseStatesUnique s
    in
    { dfa
        | states =
            result
                |> Maybe.withDefault dfa.states
        , statesValid =
            result
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False
    }


statesContainFinalStates : DFA a -> Bool
statesContainFinalStates dfa =
    let
        stateSet =
            Array.toList dfa.states
                |> List.map stateToComparable
                |> Set.fromList

        finalStateSet =
            Array.toList dfa.finalStates
                |> List.map stateToComparable
                |> Set.fromList
    in
    Set.isEmpty (Set.diff finalStateSet stateSet)


updateFinalStates : String -> DFA a -> DFA a
updateFinalStates s dfa =
    let
        result =
            parseStatesUnique s
    in
    { dfa
        | finalStates =
            result
                |> Maybe.withDefault dfa.finalStates
        , finalStatesValid =
            result
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False
    }


updateStartState : String -> DFA a -> DFA a
updateStartState s dfa =
    let
        result =
            parseStatesUnique s
    in
    { dfa
        | startState =
            case result of
                Nothing ->
                    dfa.startState

                Just array ->
                    Array.get 0 array
        , startStateValid =
            result
                |> Maybe.map (\array -> Array.length array <= 1)
                |> Maybe.withDefault False
    }


statesContainStartState : DFA a -> Bool
statesContainStartState dfa =
    let
        stateSet =
            Array.toList dfa.states
                |> List.map stateToComparable
                |> Set.fromList
    in
    case dfa.startState of
        Nothing ->
            False

        Just s ->
            Set.member (stateToComparable s) stateSet
