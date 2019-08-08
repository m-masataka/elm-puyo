module Puyo exposing (..)

type Puyo
    = Red
    | Blue
    | Green
    | Yellow
    | Ojama

puyoList = [ Red, Blue, Green, Yellow]

getPuyoImg : Puyo -> String
getPuyoImg puyo =
    case puyo of
        Red ->
            "src/img/puyo1.gif"
        Blue ->
            "src/img/puyo2.gif"
        Green ->
            "src/img/puyo3.gif"
        Yellow ->
            "src/img/puyo4.gif"
        _ ->
            "?"

