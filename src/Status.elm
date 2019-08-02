module Status exposing (..)

import Board exposing (..)

-- MODEL

type Status
    = Normal
    | Remove
    | Fall
    | New

changeStatus: Status -> List (Int, Int) -> List (Int, Int) -> List (Int, Int) -> Board -> Board -> Status
changeStatus status cgp ngp removeList board falledBoard = 
    case status of
        Normal ->
            if cgp == ngp then
                if board == falledBoard then
                    if List.length removeList > 0 then
                        Remove
                    else
                        New
                else
                    Fall
            else
                Normal
        Remove ->
            if List.length removeList > 0 then
                Remove
            else if board == falledBoard then
                New
            else
                Fall
        Fall ->
            if List.length removeList > 0 then
                Remove
            else
                New
        _ ->
            Normal
 
