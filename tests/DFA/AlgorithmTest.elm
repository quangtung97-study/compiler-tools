module DFA.AlgorithmTest exposing
    ( testComplexString
    , testSimpleString
    , testUnderscore
    , testUnrecognized
    )

import Array
import DFA.Algorithm as A
import Expect
import Test exposing (Test, describe, test)


testSimpleString : Test
testSimpleString =
    test "simple string"
        (\_ ->
            let
                result =
                    A.parseState "abcA"

                expected =
                    Just
                        (Array.fromList
                            [ { name = "abcA"
                              , subscripts = Array.empty
                              }
                            ]
                        )
            in
            Expect.equal expected result
        )


testUnrecognized : Test
testUnrecognized =
    describe "unrecognized characters"
        [ test "character +"
            (\_ -> Expect.equal Nothing (A.parseState "abc+"))
        , test "character /"
            (\_ -> Expect.equal Nothing (A.parseState "abc/"))
        ]


testUnderscore : Test
testUnderscore =
    describe "Undescore without subscript"
        [ test "single state"
            (\_ -> Expect.equal Nothing (A.parseState "abc_"))
        , test "2 state"
            (\_ -> Expect.equal Nothing (A.parseState "abc_2,3 qq_"))
        , test "double underscore"
            (\_ -> Expect.equal Nothing (A.parseState "abc_2,3qq_"))
        ]


testComplexString : Test
testComplexString =
    describe "Complex String"
        [ test "double states"
            (\_ ->
                let
                    result =
                        A.parseState "ab_1 cc_4"

                    expected =
                        Just
                            (Array.fromList
                                [ { name = "ab"
                                  , subscripts = Array.fromList [ "1" ]
                                  }
                                , { name = "cc"
                                  , subscripts = Array.fromList [ "4" ]
                                  }
                                ]
                            )
                in
                Expect.equal expected result
            )
        , test "double subscripts"
            (\_ ->
                let
                    result =
                        A.parseState "ab_1,aa cc_4,bb"

                    expected =
                        Just
                            (Array.fromList
                                [ { name = "ab"
                                  , subscripts = Array.fromList [ "1", "aa" ]
                                  }
                                , { name = "cc"
                                  , subscripts = Array.fromList [ "4", "bb" ]
                                  }
                                ]
                            )
                in
                Expect.equal expected result
            )
        ]
