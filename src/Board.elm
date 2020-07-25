module Board exposing (..)

import Types exposing (..)
import List.Extra as List


normalPuyo = [Red, Blue, Green, Yellow]
normal = [Red, Blue, Green, Yellow]


memberOfNormalPuyo : Cell -> Bool
memberOfNormalPuyo c =
    List.member c normalPuyo


replace : List (Int, Cell) -> Board -> Board
replace l b =
    case List.head l of
        Just (i, c) ->
            List.setAt i c b
            |> replace (List.tail l |> Maybe.withDefault [] )
        Nothing ->
            b

get : Int -> Board -> Cell
get i board =
    case List.getAt i board of
        Just c ->
            c
        Nothing ->
            None
