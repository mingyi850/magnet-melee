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


type alias Coordinate =
    { x : Float
    , y : Float
    }


type alias ICoordinate =
    { x : Int
    , y : Int
    }


mergeHslas : List ( Hsla, Float ) -> Hsla
mergeHslas hslaList =
    let
        softmaxStrengths =
            hslaList
                |> List.map Tuple.second
                |> sqAvg
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


hueToRadians : Float -> Float
hueToRadians hue =
    hue / 360


radiansToHue : Float -> Float
radiansToHue radians =
    radians * 360


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


getPairs : List a -> List ( a, a )
getPairs list =
    list
        |> List.indexedMap Tuple.pair
        |> List.concatMap
            (\( i, x ) ->
                list
                    |> List.drop (i + 1)
                    |> List.map (\y -> ( x, y ))
            )


euclideanDistance : Coordinate -> Coordinate -> Float
euclideanDistance coordinate1 coordinate2 =
    let
        xDiff =
            coordinate1.x - coordinate2.x

        yDiff =
            coordinate1.y - coordinate2.y
    in
    Basics.sqrt (xDiff ^ 2 + yDiff ^ 2)


coordinateToTuple : Coordinate -> ( Float, Float )
coordinateToTuple coordinate =
    ( coordinate.x, coordinate.y )


intCoordinateToTuple : ICoordinate -> ( Int, Int )
intCoordinateToTuple coordinate =
    ( coordinate.x, coordinate.y )


coordinateFromTuple : ( Float, Float ) -> Coordinate
coordinateFromTuple ( x, y ) =
    { x = x, y = y }


intCoordinateFromTuple : ( Int, Int ) -> ICoordinate
intCoordinateFromTuple ( x, y ) =
    { x = x, y = y }


intCoordinateToFloat : ICoordinate -> Coordinate
intCoordinateToFloat { x, y } =
    { x = toFloat x, y = toFloat y }


floatCoordinateToInt : Coordinate -> ICoordinate
floatCoordinateToInt { x, y } =
    { x = round x, y = round y }


coordinateFromArgs : Float -> Float -> Coordinate
coordinateFromArgs x y =
    { x = x, y = y }


roundTuple : ( Float, Float ) -> ( Int, Int )
roundTuple ( x, y ) =
    ( round x, round y )


tupleToFloat : ( Int, Int ) -> ( Float, Float )
tupleToFloat ( x, y ) =
    ( toFloat x, toFloat y )


coordinateToString : Coordinate -> String
coordinateToString coordinate =
    "(" ++ String.fromFloat coordinate.x ++ "," ++ String.fromFloat coordinate.y ++ ")"


getSurroundingCoordinates : Coordinate -> List Coordinate
getSurroundingCoordinates coordinate =
    let
        x =
            coordinate.x

        y =
            coordinate.y
    in
    [ { x = x, y = y }
    , { x = x - 1, y = y }
    , { x = x, y = y - 1 }
    , { x = x, y = y + 1 }
    , { x = x + 1, y = y }
    ]


softmax : List Float -> List Float
softmax list =
    let
        exps =
            List.map (\x -> Basics.e ^ x) list

        sum =
            List.sum exps
    in
    List.map (\x -> x / sum) exps


sqAvg : List Float -> List Float
sqAvg list =
    let
        exps =
            List.map (\x -> x ^ 2) list

        sum =
            List.sum exps
    in
    List.map (\x -> x / sum) exps
