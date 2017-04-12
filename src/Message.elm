module Message exposing (..)

import Parser
    exposing
        ( Parser
        , (|.)
        , (|=)
        , succeed
        , symbol
        , float
        , ignore
        , zeroOrMore
        , oneOrMore
        , run
        , oneOf
        , keep
        , map
        , repeat
        )
import Char


type Message
    = Phrase (List Message)
    | Str String
    | Variable String


phrase : Parser Message
phrase =
    map Phrase <|
        repeat oneOrMore
            (oneOf
                [ succeed Str
                    |= keep oneOrMore text
                , succeed Variable
                    |. symbol "{"
                    |= keep oneOrMore text
                    |. symbol "}"
                ]
            )


text : Char -> Bool
text char =
    Char.isLower char
        || Char.isUpper char
        || (char == ' ')


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


parse : String -> Result Parser.Error Message
parse string =
    run phrase string
