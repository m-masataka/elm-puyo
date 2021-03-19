module Board exposing (..)

import Types exposing (..)
import List.Extra as List


normalPuyo = [Red, Blue, Green, Yellow, Purple]
ojamaPuyo = [Ojama]

initBoard : List (Int, Cell) -> Board
initBoard puyoSet =
    let
        noneSet = List.range 0 columns |> List.map (\l -> (l * rowsLen, None))
        board = 
            Empty |> List.repeat (rowsLen * columns)
            |> replace noneSet
            |> replace puyoSet
    in
    board

memberOfPuyo : Cell -> Bool
memberOfPuyo c =
    List.member c (normalPuyo ++ ojamaPuyo)

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

stringToCell : String -> Cell
stringToCell s =
    case s of
        "blue" ->
            Blue
        "red" ->
            Red
        "green" ->
            Green
        "yellow" ->
            Yellow
        "purple" ->
            Purple
        "ojama" ->
            Ojama
        _ ->
            Blue

cellToString : Cell -> String
cellToString c =
    case c of
        Blue ->
            "blue"
        Red ->
            "red"
        Green ->
            "green"
        Yellow ->
            "yellow"
        Purple ->
            "purple"
        Ojama ->
            "ojama"
        _ ->
            "_"
            
endJudgement : Board -> Int -> Int -> List (List Cell) -> Bool
endJudgement board chain expectedChain nextPuyo =
    get (List.getAt 0 startPos |> Maybe.withDefault 0) board /= Empty
    || chain >= expectedChain
    || (List.concat nextPuyo |> List.all (\c -> c == Empty) )


{-| 
    ELM Board
    None,1 ,2 ,3 ,4 ,5 ,6 ,
    None,8 ,9 ,10,11,12,13,
    None,15,16,17,18,19,20,
    None,22,23,24,25,26,27,
    None,29,30,31,32,33,34,
    None,36,37,38,39,40,41,
    None,43,44,45,46,47,48,
    None,50,51,52,53,54,55,
    None,57,58,59,60,61,62,
    None,64,65,66,67,68,69,
    None,71,72,73,74,75,76,
    None,78,79,80,81,82,83,
    None,85,86,87,89,90,91

    API Board
    1 ,2 ,3 ,4 ,5 ,6,
    7 ,8 ,9 ,10,11,12,
    13,14,15,16,17,18,
    19,20,21,22,23,24,
    25,26,27,28,29,30,
    31,32,33,34,35,36,
    37,38,39,40,41,42,
    43,44,45,46,47,48,
    49,50,51,52,53,54,
    55,56,57,58,59,60,
    61,62,63,64,65,66,
    67,69,69,70,71,72,
    73,74,75,76,77,78
-}

mapPosition : Int -> Int
mapPosition n =
    let
        pos =
            [1 ,2 ,3 ,4 ,5 ,6 ]
         ++ [8 ,9 ,10,11,12,13]
         ++ [15,16,17,18,19,20]
         ++ [22,23,24,25,26,27]
         ++ [29,30,31,32,33,34]
         ++ [36,37,38,39,40,41]
         ++ [43,44,45,46,47,48]
         ++ [50,51,52,53,54,55]
         ++ [57,58,59,60,61,62]
         ++ [64,65,66,67,68,69]
         ++ [71,72,73,74,75,76]
         ++ [78,79,80,81,82,83]
         ++ [85,86,87,88,89,90]

    in
    List.indexedMap Tuple.pair pos
        |> List.filter (\(i, p) -> i + 1 == n )
        |> List.map (\(i, p) -> p) |> List.sum

readableCondition : String -> String -> Int -> String
readableCondition con c n =
    case con of
        "clear_c" ->
            c ++ "ぷよ全部消すべし"
        "pops_n_colors" ->
            String.fromInt(n) ++ "色消すべし"
        "pops_n_or_more_colors" ->
            String.fromInt(n) ++ "色以上消すべし"
        "pops_n_of_c" ->
            c ++ "ぷよ" ++ String.fromInt(n) ++ "個消すべし"
        "erase_n_or_more_c" ->
            c ++ "ぷよ" ++ String.fromInt(n) ++ "個以上消すべし"
        "n_chains" ->
            String.fromInt(n) ++ "連鎖すべし"
        "n_chains_or_more" ->
            String.fromInt(n) ++ "連鎖以上すべし"
        "n_chains_and_clear_c" ->
            String.fromInt(n) ++ "連鎖&" ++ c ++ "ぷよ全て消すべし"
        "n_chains_or_more_and_clear_c" ->
            String.fromInt(n) ++ "連鎖以上&" ++ c ++ "ぷよ全て消すべし"
        "pops_n_colors_simul" ->
            String.fromInt(n) ++ "色同時に消すべし"
        "pops_n_or_more_colors_simul" ->
            String.fromInt(n) ++ "色以上同時に消すべし"
        "pops_n_of_c_puyo_simul" ->
            c ++ "ぷよ" ++ String.fromInt(n) ++ "個同時に消すべし"
        "pops_n_or_more_c_puyo_simul" ->
            c ++ "ぷよ" ++ String.fromInt(n) ++ "個以上同時に消すべし"
        "pops_n_places_of_c_puyo_simul" ->
            c ++ "ぷよ" ++ String.fromInt(n) ++ "以上同時に消すべし"
        "pops_n_places_or_more_c_puyo_simul" ->
            c ++ "ぷよ" ++ String.fromInt(n) ++ "箇所以上同時に消すべし"
        "pops_c_puyo_n_chains" ->
            c ++ "ぷよ" ++ String.fromInt(n) ++ "連鎖で消すべし"
        "pops_c_puyo_n_chains_or_more" ->
            c ++ "ぷよ" ++ String.fromInt(n) ++ "連鎖以上で消すべし"
        _ ->
            "条件を見直すべし"

-- ViewBoard

updateView : List (Int, Int) -> List ViewPuyo -> List ViewPuyo
updateView l board =
    case List.head l of
        Just (i, j) ->
            List.map (\v ->
                if i == rowsLen * (round v.y + 1) + round v.x + 1 then
                    {v | end = (modBy rowsLen i - 1, (i // rowsLen - 1) + j )}
                else
                    v
            ) board
            |> updateView (List.tail l |> Maybe.withDefault [])
        Nothing ->
            board


setViewBoard : Board -> List (Int, Cell) -> List ViewPuyo
setViewBoard board_  ngp_ =
    List.indexedMap Tuple.pair board_
    |> List.append ngp_
    |> List.filter (\(i, c) -> memberOfPuyo c)
    |> List.map (\(i, c) ->
            let
                x_ = modBy rowsLen i - 1
                y_ = i // rowsLen - 1
            in
            { x = toFloat x_
            , y = toFloat y_
            , cell = c
            , imgType = Center
            , start = (x_, y_)
            , end = (x_, y_)
            })
