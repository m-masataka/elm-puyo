module KeyMgmt exposing (..)

import Json.Decode as Decode
import Types exposing (..)

emptyKeyManager : KeyManager
emptyKeyManager =
    { key = ""
    }

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
