module DFA.Algorithm exposing
    ( DFA
    , State
    , alphabet
    , emptyState
    , init
    , initState
    , parseState
    )

import Array exposing (Array)


type alias State =
    { name : String
    , subscripts : Array String
    }


initState : State
initState =
    { name = ""
    , subscripts = Array.empty
    }


type alias DFA =
    { states : Array State
    , alphabet : Array Char
    }


init : DFA
init =
    { states = Array.empty
    , alphabet = Array.empty
    }


fromAlphabetString : String -> Array Char
fromAlphabetString s =
    String.toList s
        |> List.filter (\c -> (c /= ' ') && (c /= '\t'))
        |> Array.fromList


alphabet : DFA -> String -> DFA
alphabet dfa s =
    { dfa | alphabet = fromAlphabetString s }


type ScanState
    = Init
    | Name
    | Underscore
    | Subscripts
    | Comma


emptyState : State
emptyState =
    { name = "", subscripts = Array.empty }


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


parseState : String -> Maybe (Array State)
parseState s =
    String.toList s
        |> scanRecur emptyState Init []
        |> Maybe.map
            (\( _, _, qs ) ->
                Array.fromList (List.reverse qs)
            )
