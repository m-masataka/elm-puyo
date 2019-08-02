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
            "img/puyo1.gif"
        Blue ->
            "img/puyo2.gif"
        Green ->
            "img/puyo3.gif"
        Yellow ->
            "img/puyo4.gif"
        _ ->
            "?"

