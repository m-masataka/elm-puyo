module Views exposing (..)

import Types exposing (..)
import Board exposing (..)
import Array exposing (Array)
import Html.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "game-container" ]
        [ div [ class "board-container" ]
            [ div [class "game-control-panel" ]
                [ div [ class "game-btn" ,onClick Menu ]
                         [ img [src "img/menu.png"] []
                         ]
                , div [ class "game-btn" ,onClick Pouse ]
                         [ img [src "img/stop.png" ] []
                         ]
                , div [ class "game-btn" ,onClick Start ]
                         [ img [src "img/start.png" ] []
                         ]
                , div [ class "game-btn" ,onClick Restart ]
                         [ img [src "img/restart.png" ] []
                         ]
                , div [ class "game-info" ]
                    [ img [src "img/frame.png"] []
                    , p [] [ text <| model.viewCondition ]
                    ]
                ]
            , div [ class "game-panel" ]
                [ viewNext model.nextPuyo
                , div [ class "game-board-container" ]
                    [ div [class "grid-container" ]
                        [ viewBoard model
                        ]
                    ]
                , div [class "status-panel"]
                    [ div [ class "status-frame" ]
                        [ img [src "img/frame.png"] []
                        , p [] 
                            [ span [] [text "chain"]
                            , br [][]
                            , span [] [text <| String.fromInt model.maxChain ]
                            ]
                        ]
                    ]
                ]
            , viewScore model
            , viewMenu model
            ]
        ]

viewMenu : Model -> Html Msg
viewMenu model =
    let
        modeToString : Mode -> String
        modeToString m =
            case m of
                Tokopuyo -> "とことんぷよぷよ"
                Nazopuyo -> "なぞぷよ"
    in
    case model.status of
        StartMenu ->
            let
                imgSrc = case model.mode of
                    Nazopuyo ->
                        "img/puyo_blue.gif"
                    Tokopuyo ->
                        "img/puyo_red.gif"
            in
            div [ class "menu-container" ]
                [ div [ class "mode-toggle" ]
                    [ input [ type_ "checkbox", id "mode-toggle-check", onClick ChangeMode] []
                    , label [for "mode-toggle-check"]
                        [ img [ src imgSrc ] [] 
                        , p [] [ text <| modeToString model.mode ]
                        ]
                    ]
                , viewGame model
                , div []
                    [ div [ class "game-btn", onClick Start ]
                        [ img [src "img/start.png" ] []
                        ]
                    ]
                ]
        _ ->
            div [][]

viewGame : Model -> Html Msg
viewGame model =
    case model.mode of 
        Nazopuyo ->
            div [ class "slider" ]
                [ div [ class "slides" ]
                    <| List.map viewStageInfo model.gameIndex
                ]
        Tokopuyo ->
            div [ class "rule-board"]
                [ div [] []
                ]

viewStageInfo : GameInfo -> Html Msg
viewStageInfo stageInfo =
    div [] 
        [ table [] 
            [ tr [] 
                [ td [] [text "レベル"] 
                , td [] [text stageInfo.level] 
                ]
            , tr []
                [ td [] [text "条件"] 
                , td []
                    [ text
                        <| readableCondition stageInfo.conditions.condition
                        (stageInfo.conditions.c |> Maybe.withDefault "")
                        (stageInfo.conditions.n |> Maybe.withDefault 0)
                    ] 
                ]
            , tr []
                [ td [] [
                    div [ class "game-btn", onClick (GetNewBoard stageInfo.path) ]
                        [ img [src "img/start.png" ] [] ]
                    ]
                ]
            ]
        ]


viewNext : List (List Cell) -> Html Msg
viewNext l =
    let
        width = 25
        nextcell : Cell -> Html Msg
        nextcell cell =
            div [ class "next-cell" ]
                [ 
                    case cell of
                        Empty ->
                            img [] []
                        _ ->
                            img [ src ("img/puyo_" ++ cellToString cell ++ ".png") , class "puyo" ] []
                ]
        nextSet : (Int, List (Cell))-> Html Msg
        nextSet (n ,l_) =
            div [ class "view-next"
                , style "width" (String.fromInt (width - (n * 1)) ++ "px")
                , style "margin-bottom" "10px"
                ]
                <| List.map nextcell l_
    in
    div [ class "next-puyo" ]
        <| List.map nextSet <| List.indexedMap Tuple.pair l


viewBoard : Model -> Html Msg
viewBoard model =
    let
        width = 25
        cell_ : Float -> Float -> Cell -> Html Msg
        cell_ x y cell =
            div [ class "view-cell"
                , style "position" "absolute"
                , style "top" (String.fromFloat (y * width) ++ "px")
                , style "left" (String.fromFloat ((x * width) + width) ++ "px")
                ] 
                [ img
                    [ src ("img/puyo_"++ cellToString cell ++".png")
                    , class "puyo"
                    ]
                    []
                ]
    in
    div []
        <| List.map (\c -> cell_ c.x c.y c.cell) model.viewBoard


viewScore : Model -> Html Msg
viewScore model =
    div [ class "score" ]
        [ img [src "img/frame.png"] []
        , p [] [ text <| (++) "Score : " <| String.padLeft 10 '0' <| String.fromInt model.score ]
        ]


viewEnd: Model -> Html Msg
viewEnd model =
    if model.status == End then
        div [] []
    else
        div [] []

