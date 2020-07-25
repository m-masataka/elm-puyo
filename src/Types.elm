module Types exposing (..)

import List 
import Time exposing (Posix)


rows = 6
rowsLen = 6 + 1
columns = 13
nextlen = 4
startPos = [2, 2 + rowsLen]


type alias Model =
    { board: Board
    , grippedPuyo: List (Int, Cell)
    , status: Status
    , nextPuyo: List Cell
    , timecounter: Int
    , chain : Int
    , score : Int
    }


type Status =
    Normal
    | Next
    | Fall
    | Remove
    | Stop
    | End


type Msg
    = KeyMsg KeyName
    | Tick Time.Posix
    | GenPuyo (List Cell)


type alias Board =
    List Cell


type Cell
    = Red
    | Blue
    | Green
    | Yellow
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
    }
