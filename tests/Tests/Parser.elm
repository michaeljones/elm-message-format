module Tests.Parser exposing (..)

import Expect
import Message exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Parser"
        [ describe "Str"
            [ test "All lower case" <|
                \() ->
                    Expect.equal (parse "abc") <| Ok (Phrase [ Str "abc" ])
            , test "Some lower case" <|
                \() ->
                    Expect.equal (parse "abcABC") <| Ok (Phrase [ Str "abcABC" ])
            , test "Some spaces" <|
                \() ->
                    Expect.equal (parse "abc ABC") <| Ok (Phrase [ Str "abc ABC" ])
            ]
        , describe "Variable"
            [ test "Basic variable" <|
                \() ->
                    Expect.equal (parse "abc {bob} ABC") <|
                        Ok
                            (Phrase
                                [ Str "abc "
                                , Variable "bob"
                                , Str " ABC"
                                ]
                            )
            , test "With starting variable" <|
                \() ->
                    Expect.equal (parse "{bob} ABC") <|
                        Ok
                            (Phrase
                                [ Variable "bob"
                                , Str " ABC"
                                ]
                            )
            , test "With ending variable" <|
                \() ->
                    Expect.equal (parse "ABC {bob}") <|
                        Ok
                            (Phrase
                                [ Str "ABC "
                                , Variable "bob"
                                ]
                            )
            , test "With variable in quotes" <|
                \() ->
                    Expect.equal (parse "ABC \"{bob}\" DEF") <|
                        Ok
                            (Phrase
                                [ Str "ABC \""
                                , Variable "bob"
                                , Str "\" DEF"
                                ]
                            )
            , test "With two variables" <|
                \() ->
                    Expect.equal (parse "abc {bob} something {jim} ABC") <|
                        Ok
                            (Phrase
                                [ Str "abc "
                                , Variable "bob"
                                , Str " something "
                                , Variable "jim"
                                , Str " ABC"
                                ]
                            )
            ]
        , describe "Number"
            [ test "pure example" <|
                \() ->
                    Expect.equal (parseWith number "{pctBlack, number, percent}") <|
                        Ok (Number "pctBlack" PercentDisplay)
            , test "example in phrase" <|
                \() ->
                    Expect.equal (parse "Almost {pctBlack, number, percent} of them are black.") <|
                        Ok
                            (Phrase
                                [ Str "Almost "
                                , Number "pctBlack" PercentDisplay
                                , Str " of them are black."
                                ]
                            )
            , test "example in phrase with variable" <|
                \() ->
                    Expect.equal (parse "Almost {value} {pctBlack, number, percent} of them are black.") <|
                        Ok
                            (Phrase
                                [ Str "Almost "
                                , Variable "value"
                                , Str " "
                                , Number "pctBlack" PercentDisplay
                                , Str " of them are black."
                                ]
                            )
            ]
        , describe "Plural"
            [ test "pure example" <|
                \() ->
                    let
                        string =
                            "{num_files, plural, "
                                ++ "=0{a}"
                                ++ "=1 {b} "
                                ++ "other{c}}"
                    in
                    Expect.equal (parseWith plural string) <|
                        Ok <|
                            Plural
                                "num_files"
                                [ IndexedOption 0 (Phrase [ Str "a" ])
                                , IndexedOption 1 (Phrase [ Str "b" ])
                                , OtherOption (Phrase [ Str "c" ])
                                ]
            , test "pure example with 'one'" <|
                \() ->
                    let
                        string =
                            "{num_files, plural, "
                                ++ "=0{a}"
                                ++ "one {b} "
                                ++ "other{c}}"
                    in
                    Expect.equal (parseWith plural string) <|
                        Ok <|
                            Plural
                                "num_files"
                                [ IndexedOption 0 (Phrase [ Str "a" ])
                                , OneOption (Phrase [ Str "b" ])
                                , OtherOption (Phrase [ Str "c" ])
                                ]
            , test "example with nested variable" <|
                \() ->
                    let
                        string =
                            "{num_files, plural, "
                                ++ "=0{a {x} a}"
                                ++ "=1{b {y}}"
                                ++ "other{{z} c}}"
                    in
                    Expect.equal (parseWith plural string) <|
                        Ok <|
                            Plural
                                "num_files"
                                [ IndexedOption 0 (Phrase [ Str "a ", Variable "x", Str " a" ])
                                , IndexedOption 1 (Phrase [ Str "b ", Variable "y" ])
                                , OtherOption (Phrase [ Variable "z", Str " c" ])
                                ]
            , test "multiline example" <|
                \() ->
                    let
                        string =
                            """{num_files, plural,
                                =0 {a {x} a}
                                =1 {b {y}}
                                other {{z} c}}"""
                    in
                    Expect.equal (parseWith plural string) <|
                        Ok <|
                            Plural
                                "num_files"
                                [ IndexedOption 0 (Phrase [ Str "a ", Variable "x", Str " a" ])
                                , IndexedOption 1 (Phrase [ Str "b ", Variable "y" ])
                                , OtherOption (Phrase [ Variable "z", Str " c" ])
                                ]
            ]
        ]
