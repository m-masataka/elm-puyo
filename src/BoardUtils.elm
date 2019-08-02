module BoardUtils exposing (..)

import List.Extra as ListEx
import KeyAction exposing (..)

compareRightList : List (Int, Int) -> List (Int, Int) -> Order
compareRightList la lb =
    case compare (getComparableValue la Right) (getComparableValue lb Right) of
        LT -> GT
        EQ -> EQ
        GT -> LT
 
compareLeftList : List (Int, Int) -> List (Int, Int) -> Order
compareLeftList la lb =
    case compare (getComparableValue la Left) (getComparableValue lb Left) of
        LT -> GT
        EQ -> EQ
        GT -> LT
 

compareDownList : List (Int, Int) -> List (Int, Int) -> Order
compareDownList la lb =
    case compare (getComparableValue la Down) (getComparableValue lb Down) of
        LT -> GT
        EQ -> EQ
        GT -> LT

getComparableValue : List (Int, Int)  -> KeyName -> Int
getComparableValue list  keyname =
    case ListEx.getAt 0 list of
        Just (x, y) ->
            case keyname of
                Right ->
                    x
                Left ->
                    -x
                _ ->
                    y
        Nothing ->
            -1



