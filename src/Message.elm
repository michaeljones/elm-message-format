module Message exposing (..)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore, run)


type alias Point =
    { x : Float
    , y : Float
    }


point : Parser Point
point =
    succeed Point
        |. symbol "("
        |. spaces
        |= float
        |. spaces
        |. symbol ","
        |. spaces
        |= float
        |. spaces
        |. symbol ")"


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


parse : String -> Result Parser.Error Point
parse string =
    run (point) string
