module Utils.Utils exposing (..)

import Color exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (a)


type alias Coordinate =
    { x : Float
    , y : Float
    }


type alias ICoordinate =
    { x : Int
    , y : Int
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
