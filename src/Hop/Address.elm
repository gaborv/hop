module Hop.Address exposing (..)

import Dict
import String
import Http exposing (encodeUri, decodeUri)
import Hop.Types exposing (..)


{-|
Get the Path
-}
getPath : Address -> String
getPath address =
    address.path
        |> List.map encodeUri
        |> String.join "/"
        |> String.append "/"


{-|
Get the query string from a Address.
Including ?
-}
getQuery : Address -> String
getQuery address =
    if Dict.isEmpty address.query then
        ""
    else
        address.query
            |> Dict.toList
            |> List.map (\( k, v ) -> ( encodeUri k, encodeUri v ))
            |> List.map (\( k, v ) -> k ++ "=" ++ v)
            |> String.join "&"
            |> String.append "?"



--------------------------------------------------------------------------------
-- PARSING
-- Parse a path into a Address
--------------------------------------------------------------------------------


parse : String -> Address
parse route =
    { path = parsePath route
    , query = parseQuery route
    }


extractPath : String -> String
extractPath route =
    route
        |> String.split "#"
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ""
        |> String.split "?"
        |> List.head
        |> Maybe.withDefault ""


parsePath : String -> List String
parsePath route =
    route
        |> extractPath
        |> String.split "/"
        |> List.filter (\segment -> not (String.isEmpty segment))
        |> List.map (\segment -> decodeUri segment |> Maybe.withDefault segment)


extractQuery : String -> String
extractQuery route =
    route
        |> String.split "?"
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault ""


parseQuery : String -> Query
parseQuery route =
    route
        |> extractQuery
        |> String.split "&"
        |> List.filter (not << String.isEmpty)
        |> List.map queryKVtoTuple
        |> Dict.fromList


{-| @priv
Convert a string to a tuple. Decode on the way.

    "k=1" --> ("k", "1")
-}
queryKVtoTuple : String -> ( String, String )
queryKVtoTuple kv =
    let
        splitted =
            kv
                |> String.split "="

        first =
            splitted
                |> List.head
                |> Maybe.withDefault ""

        firstDecoded =
            decodeUri first
                |> Maybe.withDefault first

        second =
            splitted
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault ""

        secondDecoded =
            decodeUri second
                |> Maybe.withDefault second
    in
        ( firstDecoded, secondDecoded )
