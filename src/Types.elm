port module Types exposing (..)

import List 
import Http
import Time exposing (Posix)
import Json.Encode as Encode 

rows = 6
rowsLen = 6 + 1
columns = 13
nextlen = 4
startPos = [2, 2 + rowsLen]


type alias Model =
    { board: Board
    , grippedPuyo: List (Int, Cell)
    , status: Status
    , nextPuyo: List (List Cell)
    , timecounter: Int
    , chain: Int
    , maxChain: Int
    , score: Int
    , viewBoard: List ViewPuyo
    , animation: Move
    , startBoard: StartBoard
    , mode: Mode
    , tmpmode: Mode
    , gameInfo: GameInfo
    , gameIndex: List GameInfo
    , viewCondition: String
    , speed: Int
    }

type alias GameInfo =
    { path : String
    , level: String
    , conditions : Condition
    }

type alias Condition =
    { condition: String
    , c: Maybe String
    , n: Maybe Int
    }

type alias StartBoard =
    { level : String
    , board : List Cells
    , next : List NextCell
    , conditions : Condition
    }

type alias Cells =
    { color : String
    , cells : List Int
    }

type alias NextCell =
    { colors : List String }


type alias ViewPuyo =
    { x : Float
    , y : Float
    , cell : Cell
    , imgType : PuyoType
    , start : (Int, Int)
    , end : (Int, Int)
    }

type Move
    = NoMove
    | FallPuyo
    | DropPuyo
    | RemovePuyo

type PuyoType
    = Center


type Status
    = Normal
    | Next
    | Fall
    | Remove
    | Stop
    | End
    | StartMenu

type Mode
    = Tokopuyo
    | Nazopuyo

type Msg
    = KeyMsg KeyName
    | Tick Time.Posix
    | GenPuyo (List Cell)
    | Pouse
    | Menu
    | Start
    | Restart
    | ChangeMode
    | InputSpeed String
    | GetNewBoard String
    | GotNewBoard (Result Http.Error StartBoard)
    | GotGameIndex (Result Http.Error (List GameInfo))


type alias Board =
    List Cell


type Cell
    = Red
    | Blue
    | Green
    | Yellow
    | Purple
    | Ojama
    | Empty
    | None


type KeyName
    = Down
    | Right
    | Left
    | SpinLeft
    | FallDown
    | Other

type alias KeyManager =
    { key : String
    , movex : Float
    , movey : Float
    }

port swipeDirectionArrow : (Encode.Value -> msg) -> Sub msg