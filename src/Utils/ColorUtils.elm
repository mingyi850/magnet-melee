module Utils.ColorUtils exposing (..)

import Color exposing (..)
import Utils.Utils exposing (..)


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


mergeHslas : List ( Hsla, Float ) -> Hsla
mergeHslas hslaList =
    let
        softmaxStrengths =
            hslaList
                |> List.map Tuple.second
                |> meanSquare
                |> List.map2 (\( hsla, _ ) softmaxStrength -> ( hsla, softmaxStrength )) hslaList
    in
    List.foldl (\( hsla, strength ) accum -> addHslaVectors (hslaToVector hsla strength) accum) zeroHslaVector softmaxStrengths
        |> hslaVectorToHsla


addHslaVectors : HslaVector -> HslaVector -> HslaVector
addHslaVectors hsla1 hsla2 =
    { x = hsla1.x + hsla2.x
    , y = hsla1.y + hsla2.y
    , z = hsla1.z + hsla2.z
    , a = Basics.max hsla1.a hsla2.a
    }


divHslaVector : Float -> HslaVector -> HslaVector
divHslaVector divisor hsla =
    { x = hsla.x / divisor
    , y = hsla.y / divisor
    , z = hsla.z / divisor
    , a = hsla.a
    }


hslaVectorToHsla : HslaVector -> Hsla
hslaVectorToHsla hslaVector =
    { hue = hueToRadians (Basics.atan2 hslaVector.y hslaVector.x * 180 / Basics.pi + 360)
    , saturation = Basics.sqrt (hslaVector.x * hslaVector.x + hslaVector.y * hslaVector.y)
    , lightness = hslaVector.z
    , alpha = hslaVector.a
    }


hslaToVector : Hsla -> Float -> HslaVector
hslaToVector hsla strength =
    { x = Basics.cos (radiansToHue hsla.hue / 180 * Basics.pi) * hsla.saturation * strength
    , y = Basics.sin (radiansToHue hsla.hue / 180 * Basics.pi) * hsla.saturation * strength
    , z = hsla.lightness * strength
    , a = hsla.alpha
    }


hueToRadians : Float -> Float
hueToRadians hue =
    hue / 360


radiansToHue : Float -> Float
radiansToHue radians =
    radians * 360


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
