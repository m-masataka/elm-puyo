import Array exposing (Array)
import Browser
import Html exposing (..)
import Browser.Events exposing (onKeyDown)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Html.Events exposing (onClick)
import Time
import Task
import Debug
import List.Extra as ListEx
import Random
import Random.List as RandomList
import Styles
import Status exposing (..)
import KeyAction exposing (..)
import Puyo exposing (..)
import Board exposing (..)
import BoardUtils exposing (..)
import FeatherIcons
 

-- Model
type alias Model = 
    { time : Time.Posix
    , score: Int
    , gripPositions: List (Int, Int)
    , board: Board
    , status: Status
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model ( Time.millisToPosix 0) 0 [(1,0),(1,1)] initBoard Normal
      , Cmd.none
    )

-- Update
type Msg
    = Tick Time.Posix
    | KeyMsg KeyName
    | RandPuyo (List Puyo)
    | Clear

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg keyname -> 
            let
                ngp = case model.status of
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
                    Down ->
                        List.sortWith compareDownList gpl
                    _ ->
                        gpl
                -- board = updateBoardByList sorted model.board
                board = updateBoardByGripPositions model.gripPositions ngp model.board
            in
            ( { model | board = board, gripPositions = ngp }
            , Cmd.none
            )
        Tick newTime ->
            let
                ngp = gripChange model.gripPositions model.board Down
                removeList = List.map (\n -> getRemoveList n model.board) puyoList
                             |> List.concat
                falledBoard = fallDown model.board
                status = changeStatus model.status model.gripPositions ngp removeList model.board falledBoard
                gpl = List.map2 Tuple.pair model.gripPositions ngp
                    |> List.map (\((x, y),(z,o)) ->
                           [(x, y), (z, o)]
                        )
                    |> List.sortWith compareDownList 
                board =
                    case status of
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
            ( { model |  board = board, gripPositions = ngp ,status = status}
            , Random.generate RandPuyo (RandomList.shuffle puyoList)
            )
        RandPuyo puyolist ->
            let
                board =
                    case model.status of
                        New -> 
                            setBoard (ListEx.getAt 0 initPositions |> Maybe.withDefault (1,0))  ( Tile ( ListEx.getAt 0 puyolist |> Maybe.withDefault Red )) model.board
                            |> setBoard (ListEx.getAt 1 initPositions |> Maybe.withDefault (1,1)) ( Tile ( ListEx.getAt 1 puyolist |> Maybe.withDefault Red ))
                        _ ->
                            model.board
                gps = 
                    case model.status of
                        New -> 
                            initPositions
                        _ ->
                            model.gripPositions
            in
            ( { model | board = board, gripPositions = gps}
            , Cmd.none
            )
        Clear ->
            ( { model | score = 0 , board = initBoard, gripPositions = initPositions, status = Normal}
            , Cmd.none
            )


removePuyo : List (Int, Int) -> Board -> Board
removePuyo list board =
    case ListEx.getAt 0 list of
        Just n ->
            setBoard n  Empty board
                 |> removePuyo (ListEx.removeAt 0 list)
        Nothing ->
            board


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
        Tile puyo ->
            div
                Styles.cellStyle
                [ img [src (getPuyoImg puyo) , width 30, height 30] []
                ]
        Empty ->
            div
                Styles.cellStyle
                [ text ""
                ]


-- SUBSCRIPTIONS
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , onKeyDown (Decode.map KeyMsg keyDecoder)
        ]

-- MAIN
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

