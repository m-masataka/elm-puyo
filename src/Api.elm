module Api exposing (..)

import Json.Decode exposing (..)
import Http
import Types exposing (..)

indexApiDecoder : Decoder (List GameInfo)
indexApiDecoder =
    Json.Decode.list infoDecoder

infoDecoder : Decoder GameInfo
infoDecoder =
    Json.Decode.map3 GameInfo
    (Json.Decode.field "path" Json.Decode.string)
    (Json.Decode.field "level" Json.Decode.string)
    (Json.Decode.field "conditions" conditionDecoder)

conditionDecoder : Decoder Condition
conditionDecoder =
    Json.Decode.map3 Condition
    (Json.Decode.field "condition" Json.Decode.string)
    (Json.Decode.field "c" (Json.Decode.nullable Json.Decode.string))
    (Json.Decode.field "n" (Json.Decode.nullable Json.Decode.int))


boardApiDecoder : Decoder StartBoard
boardApiDecoder =
    Json.Decode.map4 StartBoard
    (Json.Decode.field "level" Json.Decode.string)
    (Json.Decode.field "board" (Json.Decode.list cellsDecoder))
    (Json.Decode.field "next" (Json.Decode.list nextDecoder))
    (Json.Decode.field "conditions" conditionDecoder)

cellsDecoder : Decoder Cells
cellsDecoder =
    Json.Decode.map2 Cells
        (Json.Decode.field "color" Json.Decode.string)
        (Json.Decode.field "cells" (Json.Decode.list int))

nextDecoder : Decoder NextCell
nextDecoder =
    Json.Decode.map NextCell
        (Json.Decode.field "colors" (Json.Decode.list Json.Decode.string))

getNewBoard : String -> Cmd Msg
getNewBoard path =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ "TOKEN")
            , Http.header "Accept" "application/json"
            , Http.header "Content-Type" "application/json"
            ]
        , url = "/" ++ path
        , expect = Http.expectJson GotNewBoard boardApiDecoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }

getGameIndex : Cmd Msg
getGameIndex =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ "TOKEN")
            , Http.header "Accept" "application/json"
            , Http.header "Content-Type" "application/json"
            ]
        , url = "/api/index"
        , expect = Http.expectJson GotGameIndex indexApiDecoder
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }