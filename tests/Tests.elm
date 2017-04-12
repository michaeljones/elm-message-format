module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Message exposing (parse, Message(Str, Variable, Phrase))


all : Test
all =
    describe "Parser"
        [ describe "Example"
            [ test "All lower case" <|
                \() ->
                    Expect.equal (parse "abc") <| Ok (Phrase [ Str "abc" ])
            , test "Some lower case" <|
                \() ->
                    Expect.equal (parse "abcABC") <| Ok (Phrase [ Str "abcABC" ])
            , test "Some spaces" <|
                \() ->
                    Expect.equal (parse "abc ABC") <| Ok (Phrase [ Str "abc ABC" ])
            , test "With variable" <|
                \() ->
                    Expect.equal (parse "abc {bob} ABC") <|
                        Ok
                            (Phrase
                                [ Str "abc "
                                , Variable "bob"
                                , Str " ABC"
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
        ]
