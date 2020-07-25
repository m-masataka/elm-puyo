import Browser
import Debug
import Time
import Random
import Random.List as RandomList
import Json.Decode as Decode
import Browser.Events exposing (onKeyDown)
import List.Extra as List
import KeyMgmt exposing (..)
import Board exposing (..)
import Views exposing (..)
import Types exposing (..)
import Score exposing (..)

-- Model

init : () -> ( Model, Cmd Msg )
init _ =
    let
        lx = List.range 0 columns |> List.map (\l -> (l * rowsLen, None))
        board =
            Empty |> List.repeat (rowsLen * columns) |> Board.replace lx
    in
    ( Model board (List.zip startPos [Blue, Blue]) Normal [Red, Blue, Green, Yellow] 1 0 0
      , Cmd.none
    )


-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        move_ : KeyName -> List (Int, Cell)
        move_ keyname =
            let
                spin_ : List (Int, Cell) -> Board -> List (Int, Cell)
                spin_ l b =
                    let
                        (i1, c1) = List.getAt 0 l |> Maybe.withDefault (0, None)
                        (i2, c2) = List.getAt 1 l |> Maybe.withDefault (0, None)
                        np_ = 
                            if i1 - i2 > 0  then
                                if (i1 - i2 |> abs) == rowsLen then
                                    [ (i1, c1), (i1 - 1, c2) ]
                                else
                                    [ (i1, c1), (i1 + rowsLen, c2) ]
                            else
                                if (i1 - i2 |> abs) == rowsLen then
                                    [ (i1, c1), (i1 + 1, c2) ]
                                else
                                    [ (i1, c1), (i1 - rowsLen, c2) ]
                    in
                    if check_ np_ then
                        np_
                    else
                        [(i1, c2), (i2, c1)]

                check_ : List (Int, Cell) -> Bool
                check_ l =
                    List.map (\(i, c) -> Board.get i model.board) l |> List.all (\c -> c == Empty)

                check__ = (\l -> if check_ l then l else model.grippedPuyo)

            in
            case keyname of
                Down ->
                    check__ <| List.map (\(i, c) -> (i + rowsLen, c) ) model.grippedPuyo
                Left ->
                    check__ <| List.map (\(i, c) -> (i - 1, c) ) model.grippedPuyo
                Right ->
                    check__ <| List.map (\(i, c) -> (i + 1, c) ) model.grippedPuyo
                _ ->
                    spin_ model.grippedPuyo model.board
                    -- move__ <| (\l -> if l == model.grippedPuyo then List.reverse l else l)
                    --    <| check_ <| spin_ model.grippedPuyo model.board

    in
    case msg of
        KeyMsg keyname ->
            let
                ngp=
                    case model.status of
                        Normal ->
                            move_ keyname
                        _ ->
                            model.grippedPuyo
            in 
            ( { model | grippedPuyo = ngp }, Cmd.none )
        Tick t ->
            let
                timecounter = model.timecounter + 1
                interval = 10

                down_ : List (Int, Cell) -> Board -> Bool
                down_ l b =
                    List.map (\(i, c) -> i) l |> List.map (\i -> i + rowsLen)
                    |> List.map (\i -> Board.get i b) |> List.all (\i -> i == Empty)

                remove_ : Board -> (List (List Int), Bool)
                remove_ board_ =
                    let
                        chain_ : Int -> Cell -> List Int -> List Int
                        chain_ i c l =
                            let
                                next = [i - rowsLen, i - 1, i + 1, i + rowsLen]
                                c_ = Board.get i board_
                            in
                            if c == c_ then
                                List.filterNot (\n -> List.member n l) next
                                |> List.map (\n -> chain_ n c ( i :: l ))
                                |> List.concat |> (\li -> i :: li)
                            else
                                []

                        picked_ = List.indexedMap Tuple.pair board_
                            |> List.filterNot (\(i, c) -> c == Empty || c == None )
                            |> List.map (\(i, c) -> chain_ i c [] )
                            |> List.filter (\l -> List.length l > 3)
                    in
                    ( picked_, List.length picked_ > 0 )

                fall_ : Board -> ( Board, Bool )
                fall_ board_ =
                    let
                        fall__ : List (Int, Cell) -> Board -> Board
                        fall__ l b =
                            case List.head l of
                                Just (i, c) ->
                                    let
                                        fall___ : Int -> Int
                                        fall___ j =
                                            if Board.get (i + rowsLen * j) b == Empty then fall___ (j + 1) else j - 1
                                    in
                                    if List.member (Board.get i b) Board.normal &&
                                        Board.get (i + rowsLen) b == Empty then
                                        Board.replace [(i, Empty), (i + rowsLen * (fall___ 1), c)] b
                                        |> fall__ (List.tail l |> Maybe.withDefault [])
                                    else
                                        fall__ (List.tail l |> Maybe.withDefault []) b
                                Nothing ->
                                    b
                        
                        nb = fall__ (List.indexedMap Tuple.pair board_ |> List.reverse) board_
                    in
                    ( nb , nb /= board_)

                (ngp, board, status) =
                    case model.status of
                        Normal ->
                            if down_ model.grippedPuyo model.board then
                                if (modBy interval model.timecounter == 0) then
                                    ( move_ Down, model.board, model.status)
                                else
                                    ( model.grippedPuyo, model.board, model.status)
                            else
                                ( model.grippedPuyo, Board.replace model.grippedPuyo model.board, Fall)
                        Next ->
                            ( model.grippedPuyo, model.board, Normal)
                        Fall ->
                            let
                                (newBoard, bool) = fall_ model.board
                            in
                            if modBy interval model.timecounter == 0 then
                                ( model.grippedPuyo, newBoard, Remove)
                            else
                                ( model.grippedPuyo, model.board, if bool then model.status else Remove)
                        Remove ->
                            let
                                (list, bool) = remove_ model.board
                                rmList_ = List.map (\i -> (i, Empty)) (list |> List.concat)
                            in
                            if bool then
                                if modBy interval model.timecounter == 0 then
                                    ( model.grippedPuyo, Board.replace rmList_ model.board, Fall)
                                else
                                    ( model.grippedPuyo, model.board, model.status)
                            else
                                ( model.grippedPuyo, model.board, Next)
                        _ ->
                            ( model.grippedPuyo, model.board, model.status)

                (chain, rlist) =
                    case model.status of
                        Remove ->
                            let
                                (list, bool) = remove_ model.board
                            in
                            if modBy interval model.timecounter == 0 && bool then (model.chain + 1, list) else (model.chain, [])
                        Fall ->
                            (model.chain, [])
                        _ ->
                            (0, [])

                score = if modBy interval model.timecounter == 0 then
                        calcScore model.score
                            (rlist |> List.concat |> List.length) --  Number of Removed Puyo
                            (rlist |> List.concat |> List.unique |> List.length) -- Color Bonus
                            chain -- Chain Bonus
                            ( List.map (\l -> List.length l) rlist ) --  Linking Bonus
                    else
                        model.score

            in
            ( { model | grippedPuyo = ngp, board = board, status = status, timecounter = timecounter, chain = chain, score = score}
            , case model.status of 
                Next ->
                    Random.generate GenPuyo (RandomList.shuffle (List.append normalPuyo normalPuyo) )
                _ ->
                    Cmd.none
            )
        GenPuyo puyolist ->
            let
                (nextPuyo, status, grippedPuyo) =
                    if ( Board.get (List.getAt 0 startPos |> Maybe.withDefault 0) model.board) == Empty then
                        ( List.append (List.take 2 puyolist) (List.take 2 model.nextPuyo)
                        , Normal
                        , List.zip startPos [Board.get 2 model.nextPuyo, Board.get 3 model.nextPuyo]
                        )
                    else
                        (model.nextPuyo, End, model.grippedPuyo)
            in
            ( { model | nextPuyo = nextPuyo, status = status, grippedPuyo = grippedPuyo }
            , Cmd.none
            )


-- SUB
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map KeyMsg keyDecoder )
        , Time.every 50 Tick
        ]


-- MAIN
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
