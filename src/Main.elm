import Browser
import Debug
import Time
import Random
import Random.List as RandomList
import Browser.Events exposing (onKeyDown)
import List.Extra as List
import Json.Decode exposing (..)
import Json.Encode as Encode 
import KeyMgmt exposing (..)
import Board exposing (..)
import Views exposing (..)
import Types exposing (..)
import Score exposing (..)
import Api exposing (..)

-- Model

init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel StartMenu
    , Cmd.none
    )

initModel : Status -> Model
initModel status =
    Model (initBoard []) (List.zip startPos [Blue, Blue]) status [[Red, Blue], [Green, Yellow]] 1 0 0 0 [] NoMove initStartBoard Tokopuyo Tokopuyo initGameInfo initIndexGame ""

initGameInfo : GameInfo
initGameInfo =
    { path = ""
    , level = ""
    , conditions = initCondition
    }

initCondition : Condition
initCondition =
    { condition = ""
    , c = Nothing
    , n = Nothing
    }

initIndexGame : List GameInfo
initIndexGame =
    [ initGameInfo ]
    
initStartBoard : StartBoard
initStartBoard =
    { level = "dom"
    , board = initCells
    , next = initNext
    , conditions = initCondition
    }

initCells : List Cells
initCells =
    []

initNext : List NextCell
initNext =
    [{ colors = ["blue", "red"]},{ colors = ["red", "yellow"]}, {colors = ["red","purple"]}]


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
                        if i1 < rowsLen then
                            if i2 < rowsLen then
                                let
                                    np__ = [(i2 + rowsLen, c1), (i1 + 1, c2)]
                                in
                                if check_ np__ then
                                    np__
                                else
                                    [(i1, c2), (i2, c1)]
                            else
                                [(i1, c2), (i2, c1)]
                        else
                            [(i1, c2), (i2, c1)]

                check_ : List (Int, Cell) -> Bool
                check_ l =
                    List.map (\(i, c) -> Board.get i model.board) l |> List.all (\c -> c == Empty)

                check__ = (\l -> if check_ l then l else model.grippedPuyo)

                fallDown__ : List (Int, Cell) -> List (Int, Cell)
                fallDown__ l =
                    let
                        next__ = List.map (\(i, c) -> (i + rowsLen, c)) l
                    in
                    if check_ next__ then
                        fallDown__ <| next__
                    else
                        l

            in
            case keyname of
                Down ->
                    check__ <| List.map (\(i, c) -> (i + rowsLen, c) ) model.grippedPuyo
                Left ->
                    check__ <| List.map (\(i, c) -> (i - 1, c) ) model.grippedPuyo
                Right ->
                    check__ <| List.map (\(i, c) -> (i + 1, c) ) model.grippedPuyo
                FallDown ->
                    fallDown__ <| List.map (\(i, c) -> (i + rowsLen, c) ) model.grippedPuyo
                _ ->
                    spin_ model.grippedPuyo model.board

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

                viewBoard = Board.setViewBoard model.board ngp
            in 
            ( { model | grippedPuyo = ngp, viewBoard = viewBoard }, Cmd.none )
        Tick t ->
            let
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

                        deleteOjama : List (List Int) -> List (List Int)
                        deleteOjama l =
                            let
                                ojama_ = List.concat l
                                    |> List.unique
                                    |> List.map (\n -> [n - rowsLen, n - 1, n + 1, n + rowsLen])
                                    |> List.concat
                                    |> List.unique
                                    |> List.filter (\n -> (Board.get n board_ == Ojama))
                            in
                            l ++ [ojama_]

                        picked_ = List.indexedMap Tuple.pair board_
                            |> List.filterNot (\(i, c) -> c == Empty || c == None || c == Ojama )
                            |> List.map (\(i, c) -> chain_ i c [] )
                            |> List.filter (\l -> List.length l > 3)
                    
                    in
                    ( deleteOjama picked_, List.length picked_ > 0 )

                fall_ : Board -> (List (Int, Int), List (Int, Cell), Bool )
                fall_ board_ =
                    let
                        search_ : Int -> Int -> Board -> Int
                        search_ n i b =
                            if n < rowsLen * columns then
                                if Board.get n b == Empty then
                                    search_ (n + rowsLen) (i + 1) b
                                else
                                    search_ (n + rowsLen) i b
                            else
                                i
                        
                        moveList = List.indexedMap Tuple.pair board_ 
                            |> List.filter (\(i, c) -> memberOfPuyo c)
                            |> List.map (\(i, c) -> (i, search_ i 0 board_))
                            |> List.filter (\(i, j) -> j /= 0)
                            |> Debug.log "OB"
                        
                    in
                    ( moveList
                    , List.reverse moveList
                        |> List.map (\(i, j) -> [(i, Empty), (i + j * rowsLen, Board.get i board_)])
                        |> List.concat
                    , moveList /= []
                    )
                        

                ngp = case model.animation of
                    NoMove ->
                        case model.status of
                            Normal ->
                                if down_ model.grippedPuyo model.board then
                                    case model.mode of
                                        Tokopuyo ->
                                            move_ Down
                                        Nazopuyo ->
                                            model.grippedPuyo
                                else
                                    []
                            _ ->
                                model.grippedPuyo
                    _ ->
                        model.grippedPuyo

                (board, status, moveFlag) = case model.animation of
                    NoMove ->
                        case model.status of
                            Normal ->
                                if down_ model.grippedPuyo model.board then
                                    ( model.board, model.status, False)
                                else
                                    ( Board.replace model.grippedPuyo model.board, Fall, True)
                            Next ->
                                ( model.board, Normal, False)
                            Fall ->
                                let
                                    (moveList, newList, bool_) = fall_ model.board
                                in
                                ( Board.replace newList model.board, Remove, bool_)
                            Remove ->
                                let
                                    (list, bool_) = remove_ model.board
                                    rmList_ = List.map (\i -> (i, Empty)) (list |> List.concat)
                                in
                                if bool_ then
                                    ( Board.replace rmList_ model.board, Fall, True)
                                else
                                    ( model.board, Next, False)
                            _ ->
                                ( model.board, model.status, False)
                    _ ->
                        ( model.board, model.status, False)

                (animation, viewBoard, timecounter) =
                    case model.animation of
                        NoMove ->
                            case model.status of
                                Normal ->
                                    if status == Normal then
                                        (DropPuyo, model.viewBoard, 0)
                                    else
                                        (NoMove, model.viewBoard, 0)
                                _ ->
                                    if moveFlag then
                                        case model.status of
                                            Remove ->
                                                (RemovePuyo, model.viewBoard, 0)
                                            Fall ->
                                                let
                                                    (moveList, _, _) = fall_ model.board
                                                    viewBoard_ = Board.updateView moveList model.viewBoard
                                                in
                                                (FallPuyo, viewBoard_, 0)
                                            _ ->
                                                (DropPuyo, model.viewBoard, 0)
                                    else
                                        (NoMove, model.viewBoard, 0)
                        _ ->
                            case model.animation of
                                DropPuyo ->
                                    if model.timecounter  == 10 then
                                        (NoMove, Board.setViewBoard model.board ngp, 0)
                                    else
                                        (model.animation, model.viewBoard, model.timecounter + 1)
                                FallPuyo ->
                                    if model.timecounter == 0 then
                                        (model.animation, model.viewBoard, model.timecounter + 1)
                                    else
                                        let
                                            getY : (Int, Int) -> Int
                                            getY (x, y) =
                                                y
                                            flag = List.map (\l_ ->
                                                    getY l_.end <= ceiling l_.y
                                                ) model.viewBoard
                                                |> List.all (\b -> b)
                                        in
                                        if flag then
                                            (NoMove, Board.setViewBoard model.board ngp, 0)
                                        else
                                            ( model.animation
                                            , List.map (\l_ ->
                                                if l_.end /= (truncate l_.x, truncate l_.y) then
                                                    {l_ | y = toFloat (getY l_.start) + (0.2 * 0.5 * toFloat (model.timecounter * model.timecounter))}
                                                else
                                                    l_
                                                ) model.viewBoard 
                                            , model.timecounter + 1
                                            )
                                _ ->
                                    if model.timecounter == 10 then
                                        (NoMove, Board.setViewBoard model.board ngp, 0)
                                    else
                                        (model.animation, model.viewBoard, model.timecounter + 1)
                _ = Debug.log "status" model.status


                (chain, rlist) =
                    case model.animation of
                        NoMove ->
                            case model.status of
                                Remove ->
                                    let
                                        (list, bool) = remove_ model.board
                                    in
                                    if bool then (model.chain + 1, list) else (model.chain, [])
                                Fall ->
                                    (model.chain, [])
                                _ ->
                                    (0, [])
                        _ ->
                            (model.chain, [])

                score = calcScore model.score
                        (rlist |> List.concat |> List.length) --  Number of Removed Puyo
                        (rlist |> List.concat |> List.unique |> List.length) -- Color Bonus
                        chain -- Chain Bonus
                        ( List.map (\l -> List.length l) rlist ) --  Linking Bonus

                maxChain = if chain > model.maxChain then chain else model.maxChain
            in
            ( { model | grippedPuyo = ngp, board = board, status = status, timecounter = timecounter, chain = chain, maxChain = maxChain, score = score, viewBoard = viewBoard, animation = animation }
            , case model.status of 
                Next ->
                    Random.generate GenPuyo (RandomList.shuffle (List.append normalPuyo normalPuyo) )
                _ ->
                    Cmd.none
            )
        GenPuyo puyolist ->
            let
                (nextPuyo, status, grippedPuyo) =
                    let
                        defaultSet = List.map (\s -> Board.get s model.board ) startPos
                        dropped_ = 
                            List.indexedMap Tuple.pair model.nextPuyo |> List.drop 1 |> List.map (\(i_, pos_) -> pos_)
                        takepuyo = case model.mode of
                            Nazopuyo -> [Empty, Empty]
                            Tokopuyo -> List.take 2 puyolist |> Debug.log "naxo"
                        merged_ = [List.take 2 takepuyo]
                            |> List.append dropped_
                    in
                    if endJudgement model.board model.maxChain (model.gameInfo.conditions.n |> Maybe.withDefault 10000) model.nextPuyo then
                        (model.nextPuyo, End, model.grippedPuyo)
                    else
                        ( merged_
                        , Normal
                        , List.head model.nextPuyo |> Maybe.withDefault defaultSet |> List.zip startPos
                        )
            in
            ( { model | nextPuyo = nextPuyo, status = status, grippedPuyo = grippedPuyo }
            , Cmd.none
            )
        Pouse ->
            let
                status = case model.status of
                    Normal -> Stop
                    End -> Stop
                    _ -> model.status
            in
            ( { model | status = status }
            , Cmd.none
            )
        Menu ->
            let
                status = case model.status of
                    Normal -> StartMenu
                    End -> StartMenu
                    _ -> model.status
            in
            ( { model | status = status , tmpmode = model.mode }
            , Cmd.none
            )
        Start ->
            -- ToDo: This restart status is not always Normal.
            let
                m_ = case model.status of
                    StartMenu ->
                        case model.mode of
                            Tokopuyo ->
                                case model.tmpmode of
                                    Nazopuyo ->
                                        initModel Normal
                                    _ -> 
                                        { model | status = Normal, mode = model.tmpmode }
                            _ -> 
                                { model | status = Normal, mode = model.tmpmode }
                    _ -> 
                        { model | status = Normal, mode = model.tmpmode }
            in
            ( m_
            , Cmd.none
            )
        Restart ->
            let
                puyoSet = model.startBoard.board |> List.map (\b -> 
                        List.map (\c -> (mapPosition c, stringToCell b.color)) b.cells
                    ) |> List.concat
                board = initBoard puyoSet
                next =
                    model.startBoard.next |> List.drop 1 |> List.map (\c -> List.map stringToCell c.colors) 
                start =
                    model.startBoard.next |> List.map (\c -> List.map stringToCell c.colors) |> List.head |> Maybe.withDefault [Empty, Empty] |> List.zip startPos
            in
            ( { model | board = board, grippedPuyo = start, status = Normal, nextPuyo = next, chain = 0, maxChain = 0, score = 0, viewBoard = [], animation = NoMove, mode = model.tmpmode }
            , Cmd.none
            )
        ChangeMode ->
            let
                mode =  case model.mode of
                    Tokopuyo ->
                        Nazopuyo
                    Nazopuyo ->
                        Tokopuyo
            in
            ( { model | mode = mode }
            , getGameIndex
            )
        GetNewBoard path ->
            ( model
            , getNewBoard path
            )
        GotNewBoard result ->
            case result of
                Ok j ->
                    let
                        puyoSet = j.board |> List.map (\b -> 
                                List.map (\c -> (mapPosition c, stringToCell b.color)) b.cells
                            ) |> List.concat
                        board = initBoard puyoSet
                        next =
                            j.next |> List.drop 1 |> List.map (\c -> List.map stringToCell c.colors) 
                        start =
                            j.next |> List.map (\c -> List.map stringToCell c.colors) |> List.head |> Maybe.withDefault [Empty, Empty] |> List.zip startPos
                        viewCondition =
                            readableCondition j.conditions.condition (j.conditions.c |> Maybe.withDefault "")  (j.conditions.n |> Maybe.withDefault 0)

                    in
                    ( { model | board = board, grippedPuyo = start, status = Normal, nextPuyo = next, chain = 0, maxChain = 0, score = 0, viewBoard = [], animation = NoMove, startBoard = j, viewCondition = viewCondition }
                    , Cmd.none
                    )

                Err e ->
                    let
                        _ = Debug.log "Error" e
                    in
                    ( model, Cmd.none )
        GotGameIndex result ->
            case result of
                Ok j ->
                    ( { model | gameIndex = j }
                    , Cmd.none
                    )
                Err e ->
                    let
                        _ = Debug.log "Error" e
                    in
                    ( model, Cmd.none )


-- SUB
subscriptions model =
    Sub.batch
        [ onKeyDown (map KeyMsg keyDecoder )
        , Time.every 50 Tick
        , swipeDirectionArrow handleSwipe
        ]

handleSwipe : Encode.Value -> Msg
handleSwipe value =
    case Json.Decode.decodeValue swipeDecoder value of
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