module Utils exposing (..)

import Color exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (a)


type alias HslaVector =
    { x : Float
    , y : Float
    , z : Float
    , a : Float
    }


zeroHslaVector : HslaVector
zeroHslaVector =
    { x = 0
    , y = 0
    , z = 0
    , a = 0
    }


type alias Hsla =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }


zeroHsla : Hsla
zeroHsla =
    { hue = 0
    , saturation = 0
    , lightness = 0
    , alpha = 0
    }


maxHsla : Hsla
maxHsla =
    { hue = 0
    , saturation = 1.0
    , lightness = 1.0
    , alpha = 1.0
    }


mergeHslas : List Hsla -> Hsla
mergeHslas hslaList =
    List.foldl (\hsla accum -> addHslaVectors (hslaToVector hsla) accum) zeroHslaVector hslaList
        |> divHslaVector (toFloat (List.length hslaList))
        |> hslaVectorToHsla
        |> validateHsla


addHslaVectors : HslaVector -> HslaVector -> HslaVector
addHslaVectors hsla1 hsla2 =
    { x = hsla1.x + hsla2.x
    , y = hsla1.y + hsla2.y
    , z = hsla1.z + hsla2.z
    , a = hsla1.a + hsla2.a
    }


divHslaVector : Float -> HslaVector -> HslaVector
divHslaVector divisor hsla =
    { x = hsla.x / divisor
    , y = hsla.y / divisor
    , z = hsla.z / divisor
    , a = hsla.a / divisor
    }


validateHsla : Hsla -> Hsla
validateHsla hsla =
    if hsla.hue < 0.0 || hsla.hue > 360.0 then
        Debug.log ("hue out of range" ++ toString hsla)
            maxHsla

    else
        Debug.log ("hue in range" ++ toString hsla)
            hsla


hslaVectorToHsla : HslaVector -> Hsla
hslaVectorToHsla hslaVector =
    { hue = Basics.atan2 hslaVector.y hslaVector.x * 180 / Basics.pi + 360
    , saturation = Basics.sqrt (hslaVector.x * hslaVector.x + hslaVector.y * hslaVector.y)
    , lightness = hslaVector.z
    , alpha = hslaVector.a
    }


hslaToVector : Hsla -> HslaVector
hslaToVector hsla =
    { x = Basics.cos (hsla.hue / 180 * Basics.pi) * hsla.saturation
    , y = Basics.sin (hsla.hue / 180 * Basics.pi) * hsla.saturation
    , z = hsla.lightness
    , a = hsla.alpha
    }


addDicts : Dict Int Float -> Dict Int Float -> Dict Int Float
addDicts dict1 dict2 =
    Dict.foldl
        (\key value existing ->
            case Dict.get key existing of
                Nothing ->
                    Dict.insert key value existing

                Just value2 ->
                    Dict.insert key (value + value2) existing
        )
        dict1
        dict2


combineDicts : Dict comparable b -> Dict comparable b -> Dict comparable b
combineDicts dict1 dict2 =
    Dict.foldl (\key value existing -> Dict.insert key value existing) dict1 dict2


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


px : Int -> String
px x =
    String.fromInt x ++ "px"


pc : Int -> String
pc x =
    String.fromInt x ++ "%"
