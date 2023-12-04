module Utils exposing (..)

import Color exposing (..)
import Dict exposing (..)


addDicts : Dict Int Float -> Dict Int Float -> Dict Int Float
addDicts dict1 dict2 =
    Dict.foldl (\key value existing -> Dict.update key (\existingValue -> Maybe.map ((+) value) existingValue) existing) dict1 dict2


toCssString : Color -> String
toCssString color =
    let
        rgba =
            Color.toRgba color

        r =
            rgba.red

        g =
            rgba.green

        b =
            rgba.blue

        a =
            rgba.alpha

        pct x =
            ((x * 10000) |> round |> toFloat) / 100

        roundTo x =
            ((x * 1000) |> round |> toFloat) / 1000
    in
    "rgba("
        ++ String.fromFloat (pct r)
        ++ "%,"
        ++ String.fromFloat (pct g)
        ++ "%,"
        ++ String.fromFloat (pct b)
        ++ "%,"
        ++ String.fromFloat (roundTo a)
        ++ ")"