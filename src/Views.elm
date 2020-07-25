module Views exposing (..)

import Types exposing (..)
import Board
import Array exposing (Array)
import Html.Attributes exposing (..)
import Html exposing (..)


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "game-container" ]
        [ div [ class "board-container" ]
            [ viewNext model.nextPuyo
            , div [] [ viewBoard model ]
            , viewScore model
            ]
        ]


viewNext : List Cell -> Html Msg
viewNext l =
    div [ class "next-puyo" ]
        <| List.map viewCell l


viewBoard : Model -> Html Msg
viewBoard model =
    let
        lx = List.range 0 columns |> List.map (\a -> a * rowsLen ) 
        ly = List.range 1 columns |> List.map (\a -> a * rowsLen - 1 )
        mat = List.map2 Tuple.pair lx ly
    in
    div [ class "grid-container" ]
        [ viewEnd model
        , div [] 
            <| List.map (\(x, y) -> viewRow x y model) mat
        ]


viewRow : Int -> Int -> Model -> Html Msg
viewRow i j model =
    div [class "grid-row"] <| 
        if i /= 0 then
            let
                board = if model.status == Normal then
                        model.board |> Board.replace model.grippedPuyo
                    else
                        model.board
            in
            List.map viewCell ( board |> Array.fromList |> Array.slice (i + 1) (j + 1) |> Array.toList)
        else
            []


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Empty ->
            div [class "grid-cell"] 
                [ img [ class "puyo"
                  ]
                  []
                ]
        Red ->
            div [class "grid-cell"] 
                [ img
                    [ src "/img/puyo_1.png"
                    , class "puyo"
                    ]
                    []
                ]
        Blue ->
            div [class "grid-cell"] 
                [ img
                    [ src "/img/puyo_2.png"
                    , class "puyo"
                    ]
                    []
                ]
        Yellow ->
            div [class "grid-cell"] 
                [ img
                    [ src "/img/puyo_3.png"
                    , class "puyo"
                    ]
                    []
                ]
        Green ->
            div [class "grid-cell"] 
                [ img
                    [ src "/img/puyo_4.png"
                    , class "puyo"
                    ]
                    []
                ]
        _ -> 
            div [class "grid-cell"] 
                [ text "None"
                ]


viewScore : Model -> Html Msg
viewScore model =
    let
        list = String.fromInt model.score |> String.padLeft 10 '0' |> String.split ""
        number_ : String -> Html Msg
        number_ i =
            img [ src ("/img/"++ i ++".png"), class "number" ] []
    in
    div [ class "score" ]
        [ div [ class "score-title" ]
            [ text "Score" ]
        , div []
            <| List.map number_ list
        ]

viewEnd: Model -> Html Msg
viewEnd model =
    if model.status == End then
        div [ class "batankyu fade" ]
            [ img [ src "img/batankyu.png", class "image" ] [] ]
    else
        div [] []

