module Board exposing (..)

import Array exposing (Array)
import List.Extra as ListEx
import Puyo exposing (..)
import BoardUtils exposing (..)

type alias Board =
    Array Row


type alias Row =
    Array Cell


type Cell
    = Tile Puyo
    | Empty

initPositions = [(1,0),(1,1)]

rowLength = 6
columnLength = 13

initBoard: Board
initBoard =
    setBoardByList initPositions (Tile Red)
    <| Array.repeat columnLength <| Array.repeat rowLength <| Empty

setBoardByList : List (Int, Int) -> Cell -> Board -> Board
setBoardByList list c board =
    case ListEx.getAt 0 list of
        Just p ->
            setBoard p c board
                |> setBoardByList (ListEx.removeAt 0 list) c
        Nothing ->
            board

setBoard : (Int, Int) -> Cell -> Board -> Board
setBoard ( x, y ) cell board =
    Array.get y board
        |> Maybe.map (\oldRow -> Array.set x cell oldRow)
        |> Maybe.map (\newRow -> Array.set y newRow board)
        |> Maybe.withDefault board

fallDown : Board -> Board
fallDown board =
    let
        el = getPositionList board Empty
    in
        puyoList
            |> List.map (\n -> getPositionList board (Tile n))
            |> List.concat
            |> (\dl -> 
                   List.map (\(x, y) ->
                       List.filter (\(ex, ey) -> ex == x && ey > y ) el
                           |> List.length
                   ) dl
                   |> List.map2 Tuple.pair dl
                   |> List.map (\((x, y), num) ->
                       [(x, y), (x, y + num)]
                      )
               )
            |> List.sortWith compareDownList 
            |> (\l -> updateBoardByList l board)

updateBoardByList : List ( List (Int, Int)) -> Board -> Board
updateBoardByList list board =
    case ListEx.getAt 0 list of
        Just p ->
            updateBoard board (ListEx.getAt 0 p |> Maybe.withDefault (0, 0) ) (ListEx.getAt 1 p |> Maybe.withDefault (0, 0))
                |> updateBoardByList (ListEx.removeAt 0 list)
        Nothing ->
            board

getRemoveList : Puyo -> Board -> List (Int, Int)
getRemoveList puyo board =
    getPositionList board (Tile puyo)
    |> (\pl -> List.map (\l -> searchCell l pl ) pl)
    |> (\nlist ->  getPositionList board (Tile puyo) |> List.map2 Tuple.pair nlist)
    |> List.filterMap
        (\( num , position ) -> 
            if num > 3 then
                Just position
            else
                Nothing
        )

getPositionList : Board -> Cell ->  List (Int, Int)
getPositionList board c =
    board
        |> Array.map Array.toList
        |> Array.toList
        |> List.indexedMap (\i -> List.indexedMap (\j -> Tuple.pair ( j, i )))
        |> List.concat
        |> List.filterMap
            (\( position, cell ) ->
                  if cell == c then
                      Just position
                  else
                      Nothing
            )

searchCell : (Int, Int) -> List (Int, Int) -> Int
searchCell (x, y) list =
    let
        rlist = ListEx.elemIndex (x, y) list
            |> (\rindex -> case rindex of
                    Just ri -> 
                        ListEx.removeAt ri list
                    Nothing ->
                        list
               )
    in
    [ (x-1, y), (x+1,y), (x, y-1), (x,y+1)]
        |> List.filter (\ll -> List.member ll rlist)
        |> (\nl -> if List.length nl > 0 then
                List.map (\l -> searchCell l rlist ) nl
                |> List.sum
            else
                0
           )
        |> (\n -> n + 1)

updateBoard : Board -> (Int, Int) -> (Int, Int) -> Board
updateBoard board oldp p =
    if oldp == p then
        board
    else
        setBoard p (getPuyo oldp board)  board
            |> setBoard oldp Empty

getPuyo : (Int, Int) -> Board -> Cell
getPuyo (x, y) board =
    case Array.get y board of
        Nothing ->
            Empty
        Just row ->
            case Array.get x row of
                Nothing ->
                    Empty
                Just cell ->
                    cell

updateBoardByGripPositions : List (Int, Int) -> List (Int, Int) -> Board -> Board
updateBoardByGripPositions cgp ngp board =
    let
        cpuyo = List.map (\gp -> getPuyo gp board) cgp
    in
        setBoardByList cgp Empty board
        |> setBoard (ListEx.getAt 0 ngp |> Maybe.withDefault (0,0) ) (ListEx.getAt 0 cpuyo |> Maybe.withDefault Empty )
        |> setBoard (ListEx.getAt 1 ngp |> Maybe.withDefault (0,0) ) (ListEx.getAt 1 cpuyo |> Maybe.withDefault Empty )

spinList : List (Int, Int) -> Board -> List (Int, Int)
spinList list board =
    let
        axis = ListEx.getAt 0 list 
            |> Maybe.withDefault (0, 0)
        p = ListEx.getAt 1 list
            |> Maybe.withDefault (0, 0)
        nps = if addPosition axis (-1,0) == p then
                [ axis, addPosition axis (0, 1)]
            else if addPosition axis (0, 1) == p then
                [ axis, addPosition axis (1, 0)]
            else if addPosition axis (1, 0) == p then
                [ axis, addPosition axis (0, -1)]
            else
                [ axis, addPosition axis (-1, 0)]
        f1 = List.map (\np -> 
                setBoardByList list Empty board
                |> decideMove np
            ) nps
            |> List.sum

        nps2 = case f1 of
            0 ->
                nps
            _ ->
                [ addPosition axis (-1, 0), axis]

        f2 = List.map (\np -> 
                setBoardByList list Empty board
                |> decideMove np
            ) nps2
            |> List.sum

        nps3 = case f2 of
            0 ->
                nps2
            _ ->
                [ addPosition axis (1, 0), axis]

        f3 = List.map (\np -> 
                setBoardByList list Empty board
                |> decideMove np
            ) nps3
            |> List.sum

        nps4 = case f3 of
            0 ->
                nps3
            _ ->
                [p, axis]
    in
        nps4

addPosition : (Int, Int) -> (Int, Int) -> (Int, Int)
addPosition (x, y) (j, k) = 
    (x + j, y + k)
 
decideMove : (Int, Int) -> Board -> Int
decideMove (x, y) board =
    case Array.get y board |> Maybe.map (Array.get x) of
        Just cell ->
            case cell of 
                Just c ->
                    case c of
                        Tile number ->
                            1
                        Empty ->
                            if y < columnLength &&  x < rowLength then
                                0
                            else
                                10
                Nothing ->
                    100
        Nothing ->
            100


