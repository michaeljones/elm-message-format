module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Message exposing (parse, Point)


all : Test
all =
    describe "Parser"
        [ describe "Example"
            [ test "Point" <|
                \() ->
                    Expect.equal (parse "( 2, 3)") <| Ok (Point 2 3)
            ]
        ]
