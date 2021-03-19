module KeyMgmt exposing (..)

import Json.Decode as Decode
import Types exposing (..)

emptyKeyManager : KeyManager
emptyKeyManager =
    { key = ""
    , movex = 0
    , movey = 0
    }

keyDecoder : Decode.Decoder KeyName
keyDecoder =
    Decode.map toKeyName (Decode.field "key" Decode.string)

swipeDecoder : Decode.Decoder KeyManager
swipeDecoder =
    Decode.map3 KeyManager
        (Decode.field "key" Decode.string)
        (Decode.field "movex" Decode.float)
        (Decode.field "movey" Decode.float)

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
