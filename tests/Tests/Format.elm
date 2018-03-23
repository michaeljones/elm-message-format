module Tests.Format exposing (..)

import Dict
import Expect
import Message exposing (..)
import Test exposing (..)


(=>) : a -> b -> ( a, b )
(=>) a b =
    ( a, b )


all : Test
all =
    describe "Format"
        [ describe "Str"
            [ test "simple" <|
                \() ->
                    formatMessage Dict.empty (Str "my string")
                        |> Expect.equal (Ok "my string")
            ]
        , describe "Variable"
            [ test "missing key" <|
                \() ->
                    formatMessage Dict.empty (Variable "myKey")
                        |> Expect.equal (Err <| MissingVariableError "myKey")
            , test "key present" <|
                \() ->
                    formatMessage (Dict.fromList [ "myKey" => "myValue" ]) (Variable "myKey")
                        |> Expect.equal (Ok "myValue")
            ]
        , describe "Number"
            [ test "missing key" <|
                \() ->
                    formatMessage Dict.empty (Number "myKey" PercentDisplay)
                        |> Expect.equal (Err <| MissingVariableError "myKey")
            , test "key present" <|
                \() ->
                    formatMessage (Dict.fromList [ "myKey" => "myValue" ]) (Number "myKey" PercentDisplay)
                        |> Expect.equal (Ok "myValue")
            ]
        , describe "Plural"
            [ test "missing key" <|
                \() ->
                    formatMessage Dict.empty (Plural "myKey" [])
                        |> Expect.equal (Err <| MissingVariableError "myKey")
            , test "key present but not an int" <|
                \() ->
                    formatMessage (Dict.fromList [ "myKey" => "myValue" ]) (Plural "myKey" [])
                        |> Expect.equal (Err <| PluralKeyNotAnIntError "myValue")
            , test "no options" <|
                \() ->
                    formatMessage (Dict.fromList [ "myKey" => "3" ]) (Plural "myKey" [])
                        |> Expect.equal (Err PluralNoMatchingOptionError)
            , test "no matching option" <|
                \() ->
                    formatMessage (Dict.fromList [ "myKey" => "2" ]) (Plural "myKey" [ IndexedOption 1 (Str "myOption") ])
                        |> Expect.equal (Err PluralNoMatchingOptionError)
            , test "simple matching option" <|
                \() ->
                    formatMessage (Dict.fromList [ "myKey" => "1" ]) (Plural "myKey" [ IndexedOption 1 (Str "myOption") ])
                        |> Expect.equal (Ok "myOption")
            , test "match 'other' option" <|
                \() ->
                    formatMessage (Dict.fromList [ "myKey" => "2" ])
                        (Plural "myKey"
                            [ IndexedOption 1 (Str "myOption")
                            , OtherOption (Str "myOtherOption")
                            ]
                        )
                        |> Expect.equal (Ok "myOtherOption")
            ]
        , describe "Phrase"
            [ test "simple" <|
                \() ->
                    formatMessage Dict.empty (Phrase [ Str "myString" ])
                        |> Expect.equal (Ok "myString")
            , test "with error" <|
                \() ->
                    formatMessage Dict.empty (Phrase [ Str "myString", Variable "myKey" ])
                        |> Expect.equal (Err <| MissingVariableError "myKey")
            ]
        ]
