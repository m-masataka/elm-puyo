module Score exposing (..)

import List.Extra as ListEx
import Debug

chainBonus =
    [ 0, 0, 8, 16, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 512]

colorBonus =
    [ 0, 0, 3, 6, 12, 24]

calcScore: Int -> Int -> Int -> Int -> List Int -> Int 
calcScore score remove color chain link =
    score + (remove * 10 *
        ( 
          (ListEx.getAt chain chainBonus |> Maybe.withDefault 0 |> Debug.log "chain")
          + (List.map (\n -> n - 4 |> clamp 0 10) link |> List.sum  |> Debug.log "link")
          + (ListEx.getAt color colorBonus |> Maybe.withDefault 0|> Debug.log "color")
          |> max 1
        )
    )
