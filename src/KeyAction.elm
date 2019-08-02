module KeyAction exposing (..)

import Json.Decode as Decode

type KeyName
    = Down
    | Right
    | Left
    | Space
    | Other

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
        "Space" ->
            Space
        _ ->
            Other


