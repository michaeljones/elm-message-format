module Message exposing (..)

import Char
import Parser exposing (..)


type Message
    = Phrase (List Message)
    | Plural Value (List PluralOption)
    | Str String
    | Variable Value
    | Number Value NumberDisplay


type alias Value =
    String


type NumberDisplay
    = PercentDisplay


type PluralOption
    = IndexedOption Int Message
    | OtherOption Message



-- Parsers


str : Parser Message
str =
    succeed Str
        |= keep oneOrMore text


number : Parser Message
number =
    delayedCommitMap
        (\a b -> a)
        (succeed Number
            |. symbol "{"
            |= keep oneOrMore variableToken
            |. symbol ","
            |. spaces
            |. keyword "number"
            |. spaces
            |. symbol ","
            |. spaces
            |= percent
            |. spaces
        )
        (symbol "}")


percent : Parser NumberDisplay
percent =
    map (\_ -> PercentDisplay) (keyword "percent")


plural : Parser Message
plural =
    let
        indexedOption =
            delayedCommitMap
                (\a b -> a)
                (succeed IndexedOption
                    |. symbol "="
                    |= int
                    |. symbol "{"
                    |= lazy (\_ -> phrase)
                )
                (symbol "}")

        otherOption =
            delayedCommitMap
                (\a b -> a)
                (succeed OtherOption
                    |. keyword "other"
                    |. symbol "{"
                    |= lazy (\_ -> phrase)
                )
                (symbol "}")

        option =
            oneOf
                [ indexedOption
                , otherOption
                ]
    in
    delayedCommitMap
        (\a b -> a)
        (inContext "plural" <|
            succeed Plural
                |. symbol "{"
                |. spaces
                |= keep oneOrMore variableToken
                |. spaces
                |. symbol ","
                |. spaces
                |. keyword "plural"
                |. spaces
                |. symbol ","
                |. spaces
                |= repeat oneOrMore option
                |. spaces
        )
        (symbol "}")


variable : Parser Message
variable =
    delayedCommitMap
        (\v other -> v)
        (succeed Variable |. symbol "{" |= keep oneOrMore text)
        (symbol "}")


phrase : Parser Message
phrase =
    inContext "phrase" <|
        map Phrase <|
            repeat oneOrMore
                (oneOf
                    [ str
                    , number
                    , variable
                    , plural
                    ]
                )



-- Helpers


text : Char -> Bool
text char =
    (char /= '{')
        && (char /= '}')



-- Char.isLower char
--     || Char.isUpper char
--     || (char == ' ')
--     || List.member char ['"']


variableToken : Char -> Bool
variableToken char =
    Char.isLower char
        || Char.isUpper char
        || (char == '_')


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


parse : String -> Result Parser.Error Message
parse string =
    run phrase string


parseWith : Parser Message -> String -> Result Parser.Error Message
parseWith parser string =
    run parser string
