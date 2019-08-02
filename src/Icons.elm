module Icons
    exposing
        ( meh
        )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "24"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "24"
        ]


meh : Html msg
meh =
    svgFeatherIcon "meh"
        [ Svg.circle [ cx "12", cy "12", r "10" ] []
        , Svg.line [ x1 "8", y1 "15", x2 "16", y2 "15" ] []
        , Svg.line [ x1 "9", y1 "9", x2 "9.01", y2 "9" ] []
        , Svg.line [ x1 "15", y1 "9", x2 "15.01", y2 "9" ] []
        ]
