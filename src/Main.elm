port module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Browser.Events exposing (onKeyDown)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Html.Events exposing (onClick)
import Time
import Task
import List.Extra as ListEx
import Debug
import Random
import Random.List as RandomList
import Status exposing (..)
import KeyAction exposing (..)
import Puyo exposing (..)
import Board exposing (..)
import Score exposing (..)
import FeatherIcons
import Json.Encode as Encode 

-- Model
type alias Model = 
    { time : Time.Posix
    , score: Int
    , gripPositions: List (Int, Int)
    , board: Board
    , status: Status
    , chainCounter: Int
    , nextList: List Puyo
    , debugmsg: String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        nextPuyo = [Red, Blue, Green, Red]
    in
    ( Model ( Time.millisToPosix 0) 0 [(1,0),(1,1)] initBoard Normal 0 nextPuyo "non"
      , Cmd.none
    )

-- Update
type Msg
    = Tick Time.Posix
    | KeyMsg KeyName
    | RandPuyo (List Puyo)
    | Restart
    | ShowGuid
    | BackToGame

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
                board = updateBoardByGripPositions model.gripPositions ngp model.board
            in
            ( { model | board = board, gripPositions = ngp}
            , Cmd.none
            )
        Tick newTime ->
            let
                ngp = 
                    case model.status of
                        Guid ->
                            model.gripPositions
                        End ->
                            model.gripPositions
                        _ -> 
                            gripChange model.gripPositions model.board Down
                removeList = List.map (\n -> getRemoveList n model.board) puyoList
                falledBoard = fallDown model.board
                status =
                    changeStatus model.status model.gripPositions ngp (List.concat removeList) model.board falledBoard
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
                            List.concat removeList
                                |> (\l -> removePuyo l  model.board)
                        Fall ->
                            falledBoard
                        _ ->
                            model.board

                chainCounter = case status of
                    Remove ->
                        model.chainCounter + 1
                    Fall ->
                        model.chainCounter
                    _ ->
                        0
         
                score =
                    calcScore
                    model.score
                    (List.length (List.concat removeList) )
                    (List.filter (\l -> List.length l > 0) removeList |> List.length)
                    chainCounter
                    (List.map (\l -> List.length l) removeList)

            in
            ( { model | score = score, board = board, gripPositions = ngp ,status = status, chainCounter = chainCounter}
            , Random.generate RandPuyo (RandomList.shuffle (List.append puyoList puyoList) )
            )
        RandPuyo puyolist ->
            let
                nextList = 
                    case model.status of
                        New ->
                            List.take 2 puyolist
                            |> List.append model.nextList
                            |> List.drop 2
                        _ ->
                            model.nextList
                board =
                    case model.status of
                        New -> 
                            setBoard 
                                (ListEx.getAt 0 initPositions |> Maybe.withDefault (1,0))
                                ( Tile ( ListEx.getAt 0 model.nextList |> Maybe.withDefault Red ))
                                model.board
                            |> setBoard 
                                (ListEx.getAt 1 initPositions |> Maybe.withDefault (1,1))
                                ( Tile ( ListEx.getAt 1 model.nextList |> Maybe.withDefault Red ))
                        _ ->
                            model.board
                gps = 
                    case model.status of
                        New -> 
                            initPositions
                        _ ->
                            model.gripPositions
            in
            ( { model | board = board, gripPositions = gps, nextList = nextList }
            , Cmd.none
            )
        Restart ->
            ( { model | score = 0 , board = initBoard, gripPositions = initPositions, status = Normal, chainCounter = 0}
            , Cmd.none
            )
        ShowGuid ->
            ( { model | status = Guid }
            , Cmd.none
            )
        BackToGame ->
            ( { model | status = Normal }
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
            SpinLeft ->
                spinList list board
            FallDown ->
                List.range 1 (columnLength - 1)
                |> List.map (\n -> 
                    List.map (\(x, y) -> (x, y + n)) list
                    |> List.map (\np ->
                        setBoardByList list Empty board
                        |> decideMove np
                     )
                     |> List.sum
                   )
                |> List.filter (\n -> n == 0)
                |> List.length
                |> (\n -> List.map (\(x, y) -> (x, y + n) ) list)
            Other ->
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
       [ div [ class "heading" ]
           [ h1 [ class "title" ]
               [ text "Elm Puyo" ]
           , div [ class "scores-container" ]
               [ div [ class "score-container" ]
                   [ text (String.fromInt model.score) ]
               , div [ class "chain-container" ]
                   [ text (String.fromInt model.chainCounter) ]
               ]
           ]
       , div [ class "above-game" ]
           [ div [ class "game-intro" ]
               [ p [] [ text "Play " 
                      , strong []
                          [ text "Puyo Puyo!" ]
                      ]
               , p [] [ text "Check the "
                      , a [ href "https://en.wikipedia.org/wiki/Puyo_Puyo#Gameplay", target "_blank" ]
                          [ text "PuyoPuyo Rule" ]
                      , text " and "
                      , a [ href "#", onClick ShowGuid ]
                          [ text "Operations Guid" ]
                      ]
               ]
           , button
               [ class "restart-button"
               , onClick Restart
               ]
               [ text "Restart" ]
           ]
       , viewGameContainer model.nextList model.board
       , viewEndMessage model.status model.score
       , viewExplanation model.status
       ]

viewGameContainer : List Puyo -> Board -> Html Msg
viewGameContainer nextList board =
   div []
       [ div [class "next-puyo-container"] [ viewNextPuyo nextList ]
       , div [class "game-container"] [ viewBoard board ]
       ]

viewNextPuyo : List Puyo -> Html Msg
viewNextPuyo list =
    let
        puyo11 = ListEx.getAt 0 list |> Maybe.withDefault Red
        puyo12 = ListEx.getAt 1 list |> Maybe.withDefault Red
        puyo21 = ListEx.getAt 2 list |> Maybe.withDefault Red
        puyo22 = ListEx.getAt 3 list |> Maybe.withDefault Red
    in
    div []
        [ br [] []
        , div [] [ img [src (getPuyoImg puyo11)] [] ]
        , div [] [ img [src (getPuyoImg puyo12)] [] ]
        , br [] []
        , div [] [ img [src (getPuyoImg puyo21)] [] ]
        , div [] [ img [src (getPuyoImg puyo22)] [] ]
        ]

viewBoard : Board -> Html Msg
viewBoard board =
    div [class "grid-container"] <|
        Array.toList (Array.map viewRow board)

viewRow : Row -> Html Msg
viewRow row =
    div [class "grip-row"] <|
        Array.toList (Array.map viewCell row)

viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Tile puyo ->
            div [class "grid-cell"]
                [ img [src (getPuyoImg puyo)] []
                ]
        Empty ->
            div [class "grid-cell"]
                [ text ""
                ]

viewExplanation : Status -> Html Msg
viewExplanation status =
    div [ class ("game-explanation" ++ getGameStatus status) ]
        [ p []
            [ strong []
                [ text "HOW TO PLAY"]
            , hr [] []
            , text "Use your arrow keys or swipe and tap on a screen to move Puyo."
            , br [] []
            , br [] []
            , i [class "fas fa-arrow-circle-left"] []
            , i [class "fas fa-arrow-circle-right"] []
            , i [class "fas fa-arrow-circle-down"] []
            , text " : Move Puyo"
            , br [] []
            , br [] []
            , i [class "fas fa-arrow-circle-up"] []
            , text " or Tap  : Spin Puyo "
            ]
        , div [ class "lower" ]
            [ button
                [ class "back-button"
                , onClick BackToGame
                ]
                [ text "Back To Game" ]
            ]
        ]

viewEndMessage : Status -> Int -> Html Msg
viewEndMessage status score =
    div [ class ("game-message" ++ getGameStatus status)]
        [ p []
            [ text "Game Over" ]
        , a []
            [ text ("Score: " ++ (String.fromInt score)) ]
        , div [ class "lower" ]
            [ button
                [ class "retry-button"
                , onClick Restart
                ]
                [ text "Try again" ]
            ]
        ]

getGameStatus : Status -> String
getGameStatus status =
    case status of
        End ->
            " game-over"
        Guid ->
            " game-exp"
        _ ->
            ""

-- SUBSCRIPTIONS
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , onKeyDown (Decode.map KeyMsg keyDecoder)
        , swipeDirectionArrow handleSwipe
        ]

handleSwipe : Encode.Value -> Msg
handleSwipe value =
    case Decode.decodeValue swipeDecoder value of
        Ok keymanager ->
            KeyMsg (toKeyName keymanager.key)
        Err _ ->
            Restart

-- MAIN
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- port
port swipeDirectionArrow : (Encode.Value -> msg) -> Sub msg
