module KeyAction exposing (..)

import Json.Decode as Decode

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

emptyKeyManager : KeyManager
emptyKeyManager =
    { key = ""
    , movex = 0
    , movey = 0
    }

swipeDecoder : Decode.Decoder KeyManager
swipeDecoder =
    Decode.map3 KeyManager
        (Decode.field "key" Decode.string)
        (Decode.field "movex" Decode.float)
        (Decode.field "movey" Decode.float)

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
        "Tap" ->
            SpinLeft
        "SwipeDown" ->
            FallDown
        _ ->
            SpinLeft


