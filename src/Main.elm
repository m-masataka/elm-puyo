import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Time
import Task
import Debug
import List.Extra as ListEx
import Random
import Styles
import FeatherIcons
 

-- Model
type alias Model = 
    { time : Time.Posix
    , score: Int
    , gripPositions: List (Int, Int)
    , board: Board
    , state: Status
    }


type alias Board =
    Array Row


type alias Row =
    Array Cell


type Cell
    = Tile Int
    | Empty

type KeyName
    = Down
    | Right
    | Left
    | Space
    | Other

type Status
    = Normal
    | Remove
    | Fall
    | New

puyo = [1, 2, 3, 4]

init : () -> ( Model, Cmd Msg )
init _ =
    let
        board =
            setBoardByList [(1, 0),(1,1)] (Tile 1)
            <| Array.repeat 10 <| Array.repeat 8<| Empty
    in
    ( Model ( Time.millisToPosix 0) 0 [(1,0),(1,1)] board Normal
      , Cmd.none
    )

-- Update
type Msg
    = Tick Time.Posix
    | KeyMsg KeyName
    | RandPuyo Int
    | Clear

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg keyname -> 
            let
                ngp = case model.state of
                    Remove ->
                        model.gripPositions
                    Fall ->
                        model.gripPositions
                    _ ->
                        gripChange model.gripPositions model.board keyname
                gpl = List.map2 Tuple.pair model.gripPositions ngp
                    |> List.map (\((x, y),(z,o)) ->
                           [(x, y), (z, o)]
                        )
                sorted = case keyname of
                    Right ->
                        List.sortWith compareRightList gpl
                    Left ->
                        List.sortWith compareLeftList gpl
                    _ ->
                        List.sortWith compareFallList gpl
                board = updateBoardByList sorted model.board
            in
            ( { model | board = board, gripPositions = ngp }
            , Cmd.none
            )
        Tick newTime ->
            let
                ngp = gripChange model.gripPositions model.board Down
                removeList = List.map (\n -> getRemoveList n model.board) puyo
                             |> List.concat
                falledBoard = fallDown model.board
                state = 
                    case model.state of
                        Normal ->
                            if model.gripPositions == ngp then
                                Fall
                            else
                                Normal
                        Remove ->
                            if List.length removeList > 0 then
                                Remove
                            else if model.board == falledBoard then
                                New
                            else
                                Fall
                        Fall ->
                            Remove
                        _ ->
                            Normal
                        
                gpl = List.map2 Tuple.pair model.gripPositions ngp
                    |> List.map (\((x, y),(z,o)) ->
                           [(x, y), (z, o)]
                        )
                    |> List.sortWith compareFallList 

                board =
                    case state of
                        Normal ->
                            updateBoardByList gpl model.board
                        Remove ->
                            removeList
                                |> (\l -> removePuyo l  model.board)
                        Fall ->
                            falledBoard
                        _ ->
                            model.board

            in
            ( { model |  board = board, gripPositions = ngp ,state = state}
            , Random.generate RandPuyo (Random.int 1 4)
            )
        RandPuyo rand ->
            let
                board =
                    case model.state of
                        New -> 
                            setBoardByList [(1,0),(1,1)] (Tile rand) model.board
                        _ ->
                            model.board
                gps = 
                    case model.state of
                        New -> 
                            [(1,0),(1,1)]
                        _ ->
                            model.gripPositions
            in
            ( { model | board = board, gripPositions = gps}
            , Cmd.none
            )
        Clear ->
            let
                initBoard =
                    Array.repeat 10 <| Array.repeat 8<| Empty
                board = setBoardByList [(0, 2),(0,3)] (Tile 1) initBoard
            in
            ( { model | score = 0 , board = board, gripPositions = [(1,0),(1,1)]}
            , Cmd.none
            )

fallDown : Board -> Board
fallDown board =
    let
        el = getPositionList board Empty
    in
        puyo
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
            |> List.sortWith compareFallList 
            |> (\l -> updateBoardByList l board)

compareRightList : List (Int, Int) -> List (Int, Int) -> Order
compareRightList la lb =
    case compare (getComparableValue la Right) (getComparableValue lb Right) of
        LT -> GT
        EQ -> EQ
        GT -> LT
 
compareLeftList : List (Int, Int) -> List (Int, Int) -> Order
compareLeftList la lb =
    case compare (getComparableValue la Left) (getComparableValue lb Left) of
        LT -> GT
        EQ -> EQ
        GT -> LT
 

compareFallList : List (Int, Int) -> List (Int, Int) -> Order
compareFallList la lb =
    case compare (getComparableValue la Down) (getComparableValue lb Down) of
        LT -> GT
        EQ -> EQ
        GT -> LT

getComparableValue : List (Int, Int)  -> KeyName -> Int
getComparableValue list  keyname =
    case ListEx.getAt 0 list of
        Just (x, y) ->
            case keyname of
                Right ->
                    x
                Left ->
                    -x
                _ ->
                    y
        Nothing ->
            -1
 
updateBoardByList : List ( List (Int, Int)) -> Board -> Board
updateBoardByList list board =
    case ListEx.getAt 0 list of
        Just p ->
            updateBoard board (ListEx.getAt 0 p |> Maybe.withDefault (0, 0) ) (ListEx.getAt 1 p |> Maybe.withDefault (0, 0))
                |> updateBoardByList (ListEx.removeAt 0 list)
        Nothing ->
            board

setBoardByList : List (Int, Int) -> Cell -> Board -> Board
setBoardByList list c board =
    case ListEx.getAt 0 list of
        Just p ->
            setBoard p c board
                |> setBoardByList (ListEx.removeAt 0 list) c
        Nothing ->
            board

getRemoveList : Int -> Board -> List (Int, Int)
getRemoveList n board =
    getPositionList board (Tile n)
    |> (\pl -> List.map (\l -> searchCell l pl ) pl)
    |> (\nlist ->  getPositionList board (Tile n) |> List.map2 Tuple.pair nlist)
    |> List.filterMap
        (\( num , position ) -> 
            if num > 3 then
                Just position
            else
                Nothing
        )

canDrop : List (Int, Int) -> Board -> Bool
canDrop list board =
    let 
        nextPositions = List.map (\(x, y) -> (x, y + 1) ) list
        f = List.map (\np -> 
                setBoardByList list Empty board
                |> decideMove np
            ) nextPositions
            |> List.sum
    in 
    if f == 0 then
        True
    else
        False
       
 
removePuyo : List (Int, Int) -> Board -> Board
removePuyo list board =
    case ListEx.getAt 0 list of
        Just n ->
            setBoard n  Empty board
                 |> removePuyo (ListEx.removeAt 0 list)
        Nothing ->
            board

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


gripChange : List (Int, Int) -> Board -> KeyName -> List (Int, Int)
gripChange list board keyname =
    let
        nextPositions = case keyname of
            Down ->
                List.map (\(x, y) -> (x, y + 1) ) list
            Left ->
                List.map (\(x, y) -> (x - 1, y) ) list
            Right ->
                List.map (\(x, y) -> (x + 1, y) ) list
            _ ->
                spinList list board
        f = List.map (\np -> 
                setBoardByList list Empty board
                |> decideMove np
            ) nextPositions
            |> List.sum
    in 
        if f == 0 then
            nextPositions
        else
            list

spinList : List (Int, Int) -> Board -> List (Int, Int)
spinList list board =
    let
        axis = ListEx.getAt 0 list 
            |> Maybe.withDefault (0, 0)
        p = ListEx.getAt 1 list
            |> Maybe.withDefault (0, 0)
    in
        if addPosition axis (-1,0) == p then
            [ axis, addPosition axis (0, 1)]
        else if addPosition axis (0, 1) == p then
            [ axis, addPosition axis (1, 0)]
        else if addPosition axis (1, 0) == p then
            [ axis, addPosition axis (0, -1)]
        else
            [ axis, addPosition axis (-1, 0)]

   
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
                            if y < 10 &&  x < 8 then
                                0
                            else
                                10
                Nothing ->
                    100
        Nothing ->
            100

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

 
updateBoard : Board -> (Int, Int) -> (Int, Int) -> Board
updateBoard board oldp p =
    if oldp == p then
        board
    else
        setBoard p (getPuyo oldp board)  board
            |> setBoard oldp Empty

getPuyo : (Int, Int) -> Board -> Cell
getPuyo (x, y) board =
    let
        tile = case Array.get y board of
            Nothing ->
                Empty
            Just row ->
                case Array.get x row of
                    Nothing ->
                        Empty
                    Just cell ->
                        cell
    in
        tile

newPuyo : Board -> Int -> Board
newPuyo board r =
    setBoard (2 , 0) (Tile r) board   

setBoard : (Int, Int) -> Cell -> Board -> Board
setBoard ( x, y ) cell board =
    Array.get y board
        |> Maybe.map (\oldRow -> Array.set x cell oldRow)
        |> Maybe.map (\newRow -> Array.set y newRow board)
        |> Maybe.withDefault board



-- VIEW
view : Model -> Html Msg
view model =
   div []
       [ button [ onClick Clear ] [ text "Clear" ]
       , div [] [ text (String.fromInt model.score) ]
       , div [] [ viewBoard model.board ]
       ]


viewBoard : Board -> Html Msg
viewBoard board =
    div
        Styles.boardStyle
    <|
        Array.toList (Array.map viewRow board)


viewRow : Row -> Html Msg
viewRow row =
    div [] <|
        Array.toList (Array.map viewCell row)

viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Tile number ->
            let
                t = case number of
                    1 ->
                        "1"
                    2 ->
                        "2"
                    3 ->
                        "3"
                    4 ->
                        "4"
                    _ ->
                        "?"
            in
            div
                Styles.cellStyle
                [ text t
                ]
        Empty ->
            div
                Styles.cellStyle
                [ text ""
                ]


-- SUBSCRIPTIONS
subscriptions : model -> sub msg
subscriptions model =
    sub.batch
        [ time.every 1000 tick
        , onkeydown (decode.map keymsg keydecoder)
        ]

keyDecoder : Decode.Decoder KeyName
keyDecoder =
    Decode.map toKeyName (Decode.field "key" Decode.string)

toKeyName : String -> KeyName
toKeyName string =
    case string of
        "ArrowDown" ->
            Down
        "ArrowRight" ->
            Right
        "ArrowLeft" ->
            Left
        "Space" ->
            Space
        _ ->
            Other

-- MAIN
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

