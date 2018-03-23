module Message exposing (..)

import Char
import Dict exposing (Dict)
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
    | OneOption Message
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
                    |. spaces
                    |. symbol "{"
                    |= lazy (\_ -> phrase)
                )
                (symbol "}")

        oneOption =
            delayedCommitMap
                (\a b -> a)
                (succeed OneOption
                    |. keyword "one"
                    |. spaces
                    |. symbol "{"
                    |= lazy (\_ -> phrase)
                )
                (symbol "}")

        otherOption =
            delayedCommitMap
                (\a b -> a)
                (succeed OtherOption
                    |. keyword "other"
                    |. spaces
                    |. symbol "{"
                    |= lazy (\_ -> phrase)
                )
                (symbol "}")

        option =
            oneOf
                [ indexedOption
                , oneOption
                , otherOption
                ]
                |. spaces
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


variableToken : Char -> Bool
variableToken char =
    Char.isLower char
        || Char.isUpper char
        || (char == '_')


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')



-- Parse


parse : String -> Result Parser.Error Message
parse string =
    run phrase string


parseWith : Parser Message -> String -> Result Parser.Error Message
parseWith parser string =
    run parser string



-- Format


type Error
    = MissingVariableError String
    | PluralKeyNotAnIntError String
    | PluralNoMatchingOptionError
    | ParserError Parser.Error


formatMessage : Dict String String -> Message -> Result Error String
formatMessage dict message =
    case message of
        Phrase messages ->
            List.map (formatMessage dict) messages
                |> combineResults
                |> Result.map String.concat

        Str string ->
            Ok string

        Variable key ->
            Dict.get key dict |> Result.fromMaybe (MissingVariableError key)

        Number variable display ->
            Dict.get variable dict |> Result.fromMaybe (MissingVariableError variable)

        Plural variable options ->
            Dict.get variable dict
                |> Result.fromMaybe (MissingVariableError variable)
                |> Result.andThen (\string -> String.toInt string |> Result.mapError (\_ -> PluralKeyNotAnIntError string))
                |> Result.andThen
                    (\targetIndex ->
                        options
                            |> List.filter
                                (\option ->
                                    case option of
                                        IndexedOption index _ ->
                                            index == targetIndex

                                        OneOption _ ->
                                            targetIndex == 1

                                        OtherOption _ ->
                                            True
                                )
                            |> List.take 1
                            |> List.head
                            |> Result.fromMaybe PluralNoMatchingOptionError
                            |> Result.andThen
                                (\option ->
                                    case option of
                                        IndexedOption _ submessage ->
                                            formatMessage dict submessage

                                        OneOption submessage ->
                                            formatMessage dict submessage

                                        OtherOption submessage ->
                                            formatMessage dict submessage
                                )
                    )


formatString : Dict String String -> String -> Result Error String
formatString dict string =
    parseWith phrase string
        |> Result.mapError ParserError
        |> Result.andThen (formatMessage dict)


{-| Taken from Result.Extra
-}
combineResults : List (Result x a) -> Result x (List a)
combineResults =
    List.foldr (Result.map2 (::)) (Ok [])
