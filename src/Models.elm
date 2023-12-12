module Models exposing (..)

import Array
import CellGrid exposing (..)
import Color exposing (..)
import Common exposing (Player)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MyCellGrid exposing (..)
import Set exposing (..)
import Utils exposing (..)
import VectorUtils exposing (..)



{-------------------------------------------------
   Model and Message Types
-------------------------------------------------}


type Msg
    = CellGridMessage MyCellGrid.Msg


type PlayerColor
    = Red
    | Blue
    | Green
    | Yellow
    | White


toPlayerNumber : PlayerColor -> Int
toPlayerNumber color =
    case color of
        Red ->
            0

        Blue ->
            1

        Green ->
            2

        Yellow ->
            3

        White ->
            4


getPlayerColor : Int -> PlayerColor
getPlayerColor number =
    case number of
        0 ->
            Red

        1 ->
            Blue

        2 ->
            Green

        3 ->
            Yellow

        4 ->
            White

        _ ->
            Red


type alias MagneticField =
    { positiveVector : FloatVector
    , negativeVector : FloatVector
    , playerStrength : Dict Int Float
    }


type alias Piece =
    { player : Int
    , polarity : Polarity
    }


type alias Board =
    { pieces : Dict Int Piece
    , pieceCoordinates : Dict Int BoardCoordinate
    , coordinatePieces : Dict ( Float, Float ) Int
    , magneticField : Dict ( Int, Int ) MagneticField
    , config : BoardConfig
    , tentativePieces : Dict Int Piece
    , tentativePieceCoordinates : Dict Int BoardCoordinate
    , tentativeCoordinatePieces : Dict ( Float, Float ) Int
    , pieceVelocities : Dict Int FloatVector
    }


type alias BoardCoordinate =
    { x : Float
    , y : Float
    }


type alias IntCoordinate =
    { x : Int
    , y : Int
    }


type alias BoardConfig =
    { gridDimensions : Int
    , displaySize : Int
    }


type CellContent
    = GridPiece Piece
    | NoContent
    | GridMagneticField MagneticField
    | PieceOnField Piece MagneticField



{-------------------------------------------------
   Utility Functions
-------------------------------------------------}


playerColorToRGB : PlayerColor -> ( Float, Float, Float )
playerColorToRGB color =
    case color of
        Red ->
            ( 255, 0, 0 )

        Blue ->
            ( 0, 0, 255 )

        Green ->
            ( 0, 255, 0 )

        Yellow ->
            ( 255, 255, 0 )

        White ->
            ( 255, 255, 255 )


emptyHsla : Hsla
emptyHsla =
    { hue = 0, saturation = 0, lightness = 0, alpha = 0 }


playerColorToHslaBoard : PlayerColor -> Float -> Float -> Float -> Hsla
playerColorToHslaBoard playerColor strength totalStrength gameMagnetism =
    case playerColor of
        Red ->
            { hue = hueToRadians 0, saturation = 1, lightness = 0.6 - (strength / gameMagnetism / 4), alpha = strength / totalStrength }

        Blue ->
            { hue = hueToRadians 240, saturation = 1, lightness = 0.6 - (strength / gameMagnetism / 4), alpha = strength / totalStrength }

        Green ->
            { hue = hueToRadians 120, saturation = 1, lightness = 0.6 - (strength / gameMagnetism / 4), alpha = strength / totalStrength }

        Yellow ->
            { hue = hueToRadians 60, saturation = 1, lightness = 0.6 - (strength / gameMagnetism / 4), alpha = strength / totalStrength }

        White ->
            { hue = hueToRadians 0, saturation = 1, lightness = 1, alpha = 0 }


playerColorToHslaBoard2 : Int -> PlayerColor -> Float -> Float -> Float -> Hsla
playerColorToHslaBoard2 totalPieces playerColor strength totalStrength gameMagnetism =
    case playerColor of
        Red ->
            { hue = hueToRadians 0, saturation = 1, lightness = 0.75 - (0.5 * Basics.sqrt (strength / toFloat totalPieces / gameMagnetism)), alpha = 0.9 }

        Blue ->
            { hue = hueToRadians 240, saturation = 1, lightness = 0.75 - (0.5 * Basics.sqrt (strength / toFloat totalPieces / gameMagnetism)), alpha = 0.9 }

        Green ->
            { hue = hueToRadians 120, saturation = 1, lightness = 0.75 - (0.5 * (strength / toFloat totalPieces / gameMagnetism)), alpha = 0.9 }

        Yellow ->
            { hue = hueToRadians 60, saturation = 1, lightness = 0.75 - (0.5 * (strength / toFloat totalPieces / gameMagnetism)), alpha = 0.9 }

        White ->
            { hue = hueToRadians 0, saturation = 1, lightness = 1, alpha = 0 }


getLigthness : Float -> Float -> Float
getLigthness strength gameMagnetism =
    strength / gameMagnetism / 1.2 - 0.1


hslaToColor : Hsla -> Color.Color
hslaToColor hsla =
    fromHsla hsla


colorFromTuple : ( Float, Float, Float ) -> Color.Color
colorFromTuple ( r, g, b ) =
    Color.rgb r g b


blendRGB : List ( Float, Float, Float ) -> ( Float, Float, Float )
blendRGB colors =
    let
        ( tr, tg, tb ) =
            List.foldl (\( r, g, b ) ( r2, g2, b2 ) -> ( r + r2, g + g2, b + b2 )) ( 0, 0, 0 ) colors
    in
    ( tr / toFloat (List.length colors), tg / toFloat (List.length colors), tb / toFloat (List.length colors) )


emptyMagneticField : MagneticField
emptyMagneticField =
    { positiveVector = { x = 0, y = 0 }
    , negativeVector = { x = 0, y = 0 }
    , playerStrength = Dict.empty
    }


getPieceColor : Piece -> Color.Color
getPieceColor piece =
    let
        lightness =
            case piece.polarity of
                Positive ->
                    0.75

                Negative ->
                    0.25

                None ->
                    0.5
    in
    playerColorToHslaBoard (getPlayerColor piece.player) 1 1 1
        |> (\hsla -> { hsla | lightness = lightness, alpha = 1 })
        |> hslaToColor


type Polarity
    = Positive
    | Negative
    | None


toPolarityString : Polarity -> String
toPolarityString polarity =
    case polarity of
        Positive ->
            "Positive"

        Negative ->
            "Negative"

        None ->
            ""


fromPolarityString : String -> Polarity
fromPolarityString polarity =
    case polarity of
        "Positive" ->
            Positive

        "Negative" ->
            Negative

        _ ->
            None


toPolarityIcon : Polarity -> String
toPolarityIcon polarity =
    case polarity of
        Positive ->
            "+"

        Negative ->
            "-"

        None ->
            ""


coordinateToTuple : BoardCoordinate -> ( Float, Float )
coordinateToTuple coordinate =
    ( coordinate.x, coordinate.y )


intCoordinateToTuple : IntCoordinate -> ( Int, Int )
intCoordinateToTuple coordinate =
    ( coordinate.x, coordinate.y )


coordinateFromTuple : ( Float, Float ) -> BoardCoordinate
coordinateFromTuple ( x, y ) =
    { x = x, y = y }


intCoordinateFromTuple : ( Int, Int ) -> IntCoordinate
intCoordinateFromTuple ( x, y ) =
    { x = x, y = y }


intCoordinateToFloat : IntCoordinate -> BoardCoordinate
intCoordinateToFloat { x, y } =
    { x = toFloat x, y = toFloat y }


floatCoordinateToInt : BoardCoordinate -> IntCoordinate
floatCoordinateToInt { x, y } =
    { x = round x, y = round y }


fromArgs : Float -> Float -> BoardCoordinate
fromArgs x y =
    { x = x, y = y }


roundTuple : ( Float, Float ) -> ( Int, Int )
roundTuple ( x, y ) =
    ( round x, round y )


tupleToFloat : ( Int, Int ) -> ( Float, Float )
tupleToFloat ( x, y ) =
    ( toFloat x, toFloat y )


coordinateToString : BoardCoordinate -> String
coordinateToString coordinate =
    "(" ++ String.fromFloat coordinate.x ++ "," ++ String.fromFloat coordinate.y ++ ")"


euclideanDistance : BoardCoordinate -> BoardCoordinate -> Float
euclideanDistance coordinate1 coordinate2 =
    let
        xDiff =
            coordinate1.x - coordinate2.x

        yDiff =
            coordinate1.y - coordinate2.y
    in
    Basics.sqrt (xDiff ^ 2 + yDiff ^ 2)


emptyBoard : BoardConfig -> Board
emptyBoard boardConfig =
    { pieces = Dict.empty
    , pieceCoordinates = Dict.empty
    , coordinatePieces = Dict.empty
    , magneticField = Dict.empty
    , config = boardConfig
    , tentativePieces = Dict.empty
    , tentativePieceCoordinates = Dict.empty
    , tentativeCoordinatePieces = Dict.empty
    , pieceVelocities = Dict.empty
    }


getPieceFromCoordinate : Board -> BoardCoordinate -> Maybe Piece
getPieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.coordinatePieces
                |> Dict.get (coordinateToTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.pieces) pieceIndex


checkValidPiecePlacement : BoardCoordinate -> Board -> Bool
checkValidPiecePlacement coordinate board =
    let
        checkedCoordinates =
            getSurroundingCoordinates coordinate
    in
    List.filterMap (\coord -> getPieceFromCoordinate board coord) checkedCoordinates
        |> List.isEmpty


getSurroundingCoordinates : BoardCoordinate -> List BoardCoordinate
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


getTentativePieceFromCoordinate : Board -> BoardCoordinate -> Maybe Piece
getTentativePieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.tentativeCoordinatePieces
                |> Dict.get (coordinateToTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.tentativePieces) pieceIndex


combineMaybeVectors : FloatVector -> Maybe FloatVector -> FloatVector
combineMaybeVectors vector maybeVector =
    case maybeVector of
        Just v ->
            combineVectors vector v

        Nothing ->
            vector


updateForceDict : Dict Int FloatVector -> Dict Int FloatVector -> Dict Int FloatVector
updateForceDict forceDict newForceDict =
    Dict.foldl (\player force accumDict -> Dict.insert player (combineMaybeVectors force (Dict.get player accumDict)) accumDict) forceDict newForceDict



{-------------------------------------------------
   Magnetic Field Calculation Functions
-------------------------------------------------}


mergeMagneticFields : MagneticField -> MagneticField -> MagneticField
mergeMagneticFields field1 field2 =
    { positiveVector = { x = field1.positiveVector.x + field2.positiveVector.x, y = field1.positiveVector.y + field2.positiveVector.y }
    , negativeVector = { x = field1.negativeVector.x + field2.negativeVector.x, y = field1.negativeVector.y + field2.negativeVector.y }
    , playerStrength = addDicts field1.playerStrength field2.playerStrength
    }


updateMagneticFieldDict : List ( IntCoordinate, MagneticField ) -> Dict ( Int, Int ) MagneticField -> Dict ( Int, Int ) MagneticField
updateMagneticFieldDict fields magneticFields =
    case fields of
        [] ->
            magneticFields

        ( coordinate, field ) :: rest ->
            let
                currentField =
                    Dict.get (intCoordinateToTuple coordinate) magneticFields
            in
            case currentField of
                Just f ->
                    let
                        newField =
                            mergeMagneticFields f field
                    in
                    updateMagneticFieldDict rest (Dict.insert (intCoordinateToTuple coordinate) newField magneticFields)

                Nothing ->
                    updateMagneticFieldDict rest (Dict.insert (intCoordinateToTuple coordinate) field magneticFields)


getBoardMagneticField : Board -> Int -> Dict ( Int, Int ) MagneticField
getBoardMagneticField board magnetism =
    let
        allCoordinates =
            List.range 0 (board.config.gridDimensions - 1)
                |> List.map (\x -> List.range 0 (board.config.gridDimensions - 1) |> List.map (\y -> { x = x, y = y }))
                |> List.concat
    in
    getBoardMagneticFieldRec board magnetism allCoordinates Dict.empty


getBoardMagneticFieldRec : Board -> Int -> List IntCoordinate -> Dict ( Int, Int ) MagneticField -> Dict ( Int, Int ) MagneticField
getBoardMagneticFieldRec board magnetism remainingCoordinates magneticFields =
    case remainingCoordinates of
        [] ->
            magneticFields

        coordinate :: rest ->
            let
                currentField =
                    getMagneticFieldForCoordinate board magnetism coordinate
            in
            getBoardMagneticFieldRec board magnetism rest (Dict.insert (intCoordinateToTuple coordinate) currentField magneticFields)


getCoordinatePieces : Board -> List ( Int, BoardCoordinate, Piece )
getCoordinatePieces board =
    let
        allPieces =
            combineDicts board.pieces board.tentativePieces

        allCoordinatePieces =
            combineDicts board.coordinatePieces board.tentativeCoordinatePieces
    in
    Dict.toList allCoordinatePieces
        |> List.filterMap
            (\( coordinate, pieceIndex ) ->
                case Dict.get pieceIndex allPieces of
                    Just piece ->
                        Just ( pieceIndex, coordinateFromTuple coordinate, piece )

                    Nothing ->
                        Nothing
            )


getMagneticFieldForCoordinate : Board -> Int -> IntCoordinate -> MagneticField
getMagneticFieldForCoordinate board magnetism coordinate =
    let
        coordinatePieces =
            getCoordinatePieces board
    in
    List.foldl
        (\( _, coord, piece ) existingFields ->
            mergeMagneticFields existingFields (getFieldFromPieceAtCoordinate magnetism coordinate coord piece)
        )
        emptyMagneticField
        coordinatePieces


calculateForceVector : Int -> BoardCoordinate -> BoardCoordinate -> ( FloatVector, Float )
calculateForceVector magnetism coordinate pieceCoordinate =
    let
        eDistance =
            euclideanDistance coordinate pieceCoordinate

        distanceVector =
            { x = coordinate.x - pieceCoordinate.x, y = coordinate.y - pieceCoordinate.y }

        distanceVectorSum =
            abs distanceVector.x + abs distanceVector.y

        magnetStrength =
            if eDistance == 0 then
                0

            else
                toFloat magnetism / (eDistance ^ 2)

        resultantVector =
            { x = distanceVector.x * magnetStrength / distanceVectorSum
            , y = distanceVector.y * magnetStrength / distanceVectorSum
            }
    in
    ( resultantVector, magnetStrength )


getFieldFromPieceAtCoordinate : Int -> IntCoordinate -> BoardCoordinate -> Piece -> MagneticField
getFieldFromPieceAtCoordinate magnetism coordinate pieceCoordinate piece =
    if intCoordinateToFloat coordinate == pieceCoordinate then
        emptyMagneticField

    else
        let
            floatCoordinate =
                intCoordinateToFloat coordinate

            ( resultantVector, magnetStrength ) =
                calculateForceVector magnetism floatCoordinate pieceCoordinate
        in
        case piece.polarity of
            Positive ->
                { positiveVector = resultantVector, negativeVector = { x = 0, y = 0 }, playerStrength = Dict.fromList [ ( piece.player, magnetStrength ) ] }

            Negative ->
                { positiveVector = { x = 0, y = 0 }, negativeVector = resultantVector, playerStrength = Dict.fromList [ ( piece.player, magnetStrength ) ] }

            None ->
                { positiveVector = { x = 0, y = 0 }, negativeVector = { x = 0, y = 0 }, playerStrength = Dict.empty }


updateBoardMagneticField : Int -> Board -> Board
updateBoardMagneticField magnetism board =
    { board
        | magneticField = getBoardMagneticField board magnetism
    }


updatePiecePositions : Float -> Board -> Board
updatePiecePositions friction board =
    let
        coordinatePieces =
            Dict.values (Dict.map (\coordinate index -> ( coordinate, index, Dict.get index board.pieces )) board.coordinatePieces)
    in
    updatePiecePositionsRecursive friction coordinatePieces board.magneticField board


updatePiecePositions2 : Float -> Board -> Board
updatePiecePositions2 friction board =
    let
        coordinatePieces =
            Dict.values (Dict.map (\coordinate index -> ( coordinate, index, Dict.get index board.pieces )) board.coordinatePieces)
    in
    updatePiecePositionsRecursive2 friction coordinatePieces board


updatePiecePositionsRecursive : Float -> List ( ( Float, Float ), Int, Maybe Piece ) -> Dict ( Int, Int ) MagneticField -> Board -> Board
updatePiecePositionsRecursive friction coordinatePieces magneticFields board =
    case coordinatePieces of
        [] ->
            board

        coordinatePiece :: rest ->
            let
                ( coordinate, pieceIndex, piece ) =
                    coordinatePiece

                field =
                    Dict.get (roundTuple coordinate) magneticFields
            in
            case piece of
                Just p ->
                    case field of
                        Just f ->
                            let
                                movementVector =
                                    scaledIntUnit 1 (getMovementVectorForMagnet friction friction f p)
                            in
                            updatePiecePositionsRecursive friction rest magneticFields (movePiece board pieceIndex movementVector)

                        Nothing ->
                            updatePiecePositionsRecursive friction rest magneticFields board

                Nothing ->
                    updatePiecePositionsRecursive friction rest magneticFields board


updatePiecePositionsRecursive2 : Float -> List ( ( Float, Float ), Int, Maybe Piece ) -> Board -> Board
updatePiecePositionsRecursive2 friction coordinatePieces board =
    case coordinatePieces of
        [] ->
            board

        coordinatePiece :: rest ->
            let
                ( coordinate, pieceIndex, piece ) =
                    coordinatePiece

                velocity =
                    Dict.get pieceIndex board.pieceVelocities
            in
            case piece of
                Just p ->
                    case velocity of
                        Just f ->
                            let
                                movementVector =
                                    floatCoordinateToInt (multiplyVector 0.01 f)
                            in
                            updatePiecePositionsRecursive2 friction rest (movePiece board pieceIndex movementVector)

                        Nothing ->
                            updatePiecePositionsRecursive2 friction rest board

                Nothing ->
                    updatePiecePositionsRecursive2 friction rest board


getFrictionVector : Float -> FloatVector -> FloatVector
getFrictionVector friction vector =
    let
        xMagnitude =
            abs vector.x

        yMagnitude =
            abs vector.y

        totalMagnitude =
            xMagnitude + yMagnitude

        xFriction =
            friction * xMagnitude

        yFriction =
            friction * yMagnitude

        xSign =
            if vector.x < 0 then
                1

            else
                -1

        ySign =
            if vector.y < 0 then
                1

            else
                -1
    in
    { x = xFriction * xSign, y = yFriction * ySign }


updatePieceCollisions : Board -> Board
updatePieceCollisions board =
    let
        updates =
            getCollidingPieceUpdates2 board board.pieceVelocities

        log2 =
            Debug.log ("updates" ++ toString updates) 1

        newPieceVelocities =
            updateForceDict board.pieceVelocities updates

        log =
            Debug.log ("afterCollision" ++ toString newPieceVelocities) 1
    in
    { board
        | pieceVelocities = newPieceVelocities
    }


checkAllVectorStop : Board -> Board
checkAllVectorStop board =
    let
        newPieceVelocities =
            Dict.map (\i v -> checkVectorStop v) board.pieceVelocities
    in
    { board
        | pieceVelocities = newPieceVelocities
    }


checkVectorStop : FloatVector -> FloatVector
checkVectorStop vector =
    if sqrt (vector.x ^ 2 + vector.y ^ 2) < 50 then
        { x = 0, y = 0 }

    else
        vector


updatePieceVelocities : Int -> Board -> Board
updatePieceVelocities magnetism board =
    let
        friction =
            0.4

        magneticForceDict =
            getMagneticForceOnAllPieces magnetism board

        log =
            Debug.log ("magneticForceDict" ++ toString magneticForceDict) 1

        newVelocities =
            updateForceDict board.pieceVelocities magneticForceDict

        log2 =
            Debug.log ("newVelocities" ++ toString newVelocities) 1

        newVelocitiesWithFriction =
            Dict.map
                (\index vector -> combineVectors (getFrictionVector friction vector) vector)
                newVelocities

        log3 =
            Debug.log ("newVelocitiesAfterFriction" ++ toString newVelocitiesWithFriction) 1
    in
    { board
        | pieceVelocities = newVelocitiesWithFriction
    }
        |> updatePieceCollisions
        |> checkAllVectorStop


zeroPieceVelocities : Board -> Board
zeroPieceVelocities board =
    { board
        | pieceVelocities = Dict.map (\i v -> { x = 0, y = 0 }) board.pieceVelocities
    }



{- getCollidingPieceUpdates : Board -> Float -> Dict Int FloatVector -> Dict Int FloatVector
   getCollidingPieceUpdates board friction velocities =
       let
           collidingPieces =
               getCollidingPieces board

           velocitesAfterCollision =
               List.foldl
                   (\( ( i1, coord1, piece1 ), ( i2, coord2, piece2 ) ) accumDict ->
                       let
                           relativePosition =
                               { x = coord1.x - coord2.x, y = coord1.y - coord2.y }

                           resultantDict =
                               case Dict.get i1 velocities of
                                   Just v1 ->
                                       case Dict.get i2 velocities of
                                           Just v2 ->
                                               Dict.fromList [ ( i1, v2 ), ( i2, v1 ) ]

                                           Nothing ->
                                               Dict.empty

                                   Nothing ->
                                       Dict.empty
                       in
                       updateForceDict accumDict resultantDict
                   )
                   velocities
                   collidingPieces

           collidedPieces =
               getCollidedPiecesFromPairs collidingPieces

           velocitiesAfterFriction =
               Dict.map
                   (\index vector ->
                       if Set.member index collidedPieces then
                           combineVectors (getFrictionVector friction vector) vector

                       else
                           vector
                   )
                   velocitesAfterCollision
       in
       velocitiesAfterFriction
-}


getCollidingPieceUpdates : Board -> Float -> Dict Int FloatVector -> Dict Int FloatVector
getCollidingPieceUpdates board friction velocities =
    let
        collidingPieces =
            getCollidingPieces board

        velocitiesAfterCollision =
            List.foldl
                (\( ( i1, _, _ ), ( i2, _, _ ) ) accumDict ->
                    case ( Dict.get i1 velocities, Dict.get i2 velocities ) of
                        ( Just v1, Just v2 ) ->
                            let
                                averageVelocity =
                                    { x = (v1.x + v2.x) / 2, y = (v1.y + v2.y) / 2 }
                            in
                            accumDict
                                |> Dict.insert i1 averageVelocity
                                |> Dict.insert i2 averageVelocity

                        _ ->
                            accumDict
                )
                velocities
                collidingPieces

        collidedPieces =
            getCollidedPiecesFromPairs collidingPieces

        velocitiesAfterFriction =
            Dict.map
                (\index vector ->
                    if Set.member index collidedPieces then
                        combineVectors (getFrictionVector friction vector) vector

                    else
                        vector
                )
                velocitiesAfterCollision
    in
    velocitiesAfterFriction


getCollidingPieceUpdates2 : Board -> Dict Int FloatVector -> Dict Int FloatVector
getCollidingPieceUpdates2 board velocities =
    let
        collidingPieces =
            getCollidingPieces board

        velocitiesAfterCollision =
            List.foldl
                (\( ( i1, c1, _ ), ( i2, c2, _ ) ) accumDict ->
                    case ( Dict.get i1 velocities, Dict.get i2 velocities ) of
                        ( Just v1, Just v2 ) ->
                            let
                                proportion =
                                    vectorMagnitude v1 / (vectorMagnitude v1 + vectorMagnitude v2)

                                impulse =
                                    calculateImpulse 1 v1 v2 (calculateCollisionNormal c1 c2)

                                antiV1 =
                                    multiplyVector proportion impulse

                                antiV2 =
                                    multiplyVector (1 - proportion) (negative impulse)

                                log =
                                    Debug.log ("Impulse for " ++ toString c1 ++ toString c2 ++ toString impulse ++ "proportion" ++ toString proportion ++ "antiV1" ++ toString antiV1 ++ "antiV2" ++ toString antiV2) 1
                            in
                            updateForceDict (Dict.fromList [ ( i1, antiV1 ), ( i2, antiV2 ) ]) accumDict

                        _ ->
                            accumDict
                )
                Dict.empty
                collidingPieces
    in
    velocitiesAfterCollision


calculateImpulse : Float -> FloatVector -> FloatVector -> FloatVector -> FloatVector
calculateImpulse restitution velocity1 velocity2 collisionNormal =
    let
        relativeVelocity =
            { x = velocity1.x - velocity2.x, y = velocity1.y - velocity2.y }

        velocityScaleAlongNormal =
            relativeVelocity.x * collisionNormal.x + relativeVelocity.y * collisionNormal.y

        velocityAlongNormalVector =
            { x = restitution * velocityScaleAlongNormal * collisionNormal.x, y = restitution * velocityScaleAlongNormal * collisionNormal.y }

        impulse =
            if velocityScaleAlongNormal > 0 then
                negative velocityAlongNormalVector
                --{ x = -restitution * relativeVelocity.x, y = -restitution * relativeVelocity.y }

            else
                { x = 0, y = 0 }
    in
    impulse


calculateCollisionNormal : BoardCoordinate -> BoardCoordinate -> FloatVector
calculateCollisionNormal position1 position2 =
    let
        directionVector =
            { x = position2.x - position1.x, y = position2.y - position1.y }

        magnitude =
            sqrt (directionVector.x ^ 2 + directionVector.y ^ 2)

        collisionNormal =
            { x = directionVector.x / magnitude, y = directionVector.y / magnitude }
    in
    collisionNormal


getCollidedPiecesFromPairs : List ( ( Int, BoardCoordinate, Piece ), ( Int, BoardCoordinate, Piece ) ) -> Set Int
getCollidedPiecesFromPairs pairs =
    List.foldl
        (\( ( i1, coord1, piece1 ), ( i2, coord2, piece2 ) ) accum ->
            [ i1, i2 ] ++ accum
        )
        []
        pairs
        |> Set.fromList


getCollidingPieces : Board -> List ( ( Int, BoardCoordinate, Piece ), ( Int, BoardCoordinate, Piece ) )
getCollidingPieces board =
    let
        coordinatePieces =
            getCoordinatePieces board

        coordinatePairs =
            getPairs coordinatePieces
    in
    List.filter
        (\( ( i1, coord1, piece1 ), ( i2, coord2, piece2 ) ) ->
            let
                distance =
                    euclideanDistance coord1 coord2
            in
            distance <= 2
        )
        coordinatePairs


getMagneticForceOnAllPieces : Int -> Board -> Dict Int FloatVector
getMagneticForceOnAllPieces magnetism board =
    let
        coordinatePieces =
            getCoordinatePieces board

        coordinatePiecesWithoutTentative =
            List.filter (\( index, _, _ ) -> not (Dict.member index board.tentativePieces)) coordinatePieces

        coordinatePairs =
            getPairs coordinatePiecesWithoutTentative
    in
    List.foldl
        (\( ( i1, coord1, piece1 ), ( i2, coord2, piece2 ) ) accumDict ->
            let
                force =
                    getMagneticForceBetweenPieces magnetism coord1 piece1 coord2 piece2

                resultantDict =
                    Dict.fromList [ ( i1, force ), ( i2, negative force ) ]
            in
            updateForceDict accumDict resultantDict
        )
        Dict.empty
        coordinatePairs


getMagneticForceBetweenPieces : Int -> BoardCoordinate -> Piece -> BoardCoordinate -> Piece -> FloatVector
getMagneticForceBetweenPieces magnetism coordinate1 piece1 coordinate2 piece2 =
    let
        ( force, totalStrength ) =
            calculateForceVector magnetism coordinate1 coordinate2
    in
    if piece1.polarity == piece2.polarity then
        force
        {- else if
               euclideanDistance coordinate1 coordinate2 <= 2
               --pieces are touching, negate attraction forces
           then
               { x = 0, y = 0 }
        -}

    else
        negative force


getMovementVectorForMagnet : Float -> Float -> MagneticField -> Piece -> IntVector
getMovementVectorForMagnet friction magnitude field piece =
    let
        resultantVector =
            case piece.polarity of
                Positive ->
                    combineVectors field.positiveVector (negative field.negativeVector)

                Negative ->
                    combineVectors field.negativeVector (negative field.positiveVector)

                None ->
                    { x = 0, y = 0 }

        xMagnitude =
            abs resultantVector.x

        yMagnitude =
            abs resultantVector.y

        resultantX =
            if xMagnitude > friction then
                round resultantVector.x

            else
                0

        resultantY =
            if yMagnitude > friction then
                round resultantVector.y

            else
                0
    in
    { x = resultantX, y = resultantY }



{-------------------------------------------------
   Piece Movement and Insertion Functions
-------------------------------------------------}


insertPiece : Piece -> BoardCoordinate -> Board -> Board
insertPiece piece coordinate board =
    let
        pieceIndex =
            Maybe.withDefault 0 (List.maximum (Dict.keys board.pieces)) + 1
    in
    { board
        | pieceCoordinates = Dict.insert pieceIndex coordinate board.pieceCoordinates
        , coordinatePieces = Dict.insert (coordinateToTuple coordinate) pieceIndex board.coordinatePieces
        , pieces = Dict.insert pieceIndex piece board.pieces
    }


insertTentativePiece : Piece -> BoardCoordinate -> Board -> Board
insertTentativePiece piece coordinate board =
    let
        pieceIndex =
            9999
    in
    { board
        | tentativePieceCoordinates = Dict.fromList [ ( pieceIndex, coordinate ) ]
        , tentativeCoordinatePieces = Dict.fromList [ ( coordinateToTuple coordinate, pieceIndex ) ]
        , tentativePieces = Dict.fromList [ ( pieceIndex, piece ) ]
    }


removeTentativePieces : Board -> Board
removeTentativePieces board =
    { board
        | tentativePieceCoordinates = Dict.empty
        , tentativeCoordinatePieces = Dict.empty
        , tentativePieces = Dict.empty
    }


movePieceCoordinate : BoardCoordinate -> IntVector -> BoardCoordinate
movePieceCoordinate coordinate vector =
    { x = coordinate.x + toFloat vector.x, y = coordinate.y + toFloat vector.y }


getMaxMovementCoordinate : Int -> Board -> BoardCoordinate -> IntVector -> IntVector -> BoardCoordinate
getMaxMovementCoordinate pieceIndex board coordinate vector prevVector =
    let
        unitVector =
            unitIntVector vector

        tentativeNext =
            movePieceCoordinate coordinate (unitIntVector prevVector)

        tentativeNextPiece =
            getPieceFromCoordinate board tentativeNext

        actualNext =
            movePieceCoordinate coordinate unitVector
    in
    case tentativeNextPiece of
        Just _ ->
            Debug.log ("Max movement coordinate: found next piece at tentativeNext" ++ toString tentativeNext ++ "original" ++ toString prevVector ++ "will move in dir" ++ toString (negativeIntVec prevVector))
                movePieceCoordinate
                coordinate
                (negativeIntVec (unitIntVector prevVector))

        Nothing ->
            getMaxMovementCoordinate pieceIndex (movePieceUnsafe board pieceIndex unitVector) actualNext (decreaseIntVectorMagnitude vector) vector


movePieceUnsafe : Board -> Int -> IntVector -> Board
movePieceUnsafe board pieceIndex vector =
    let
        pieceCoordinate =
            Dict.get pieceIndex board.pieceCoordinates
    in
    case pieceCoordinate of
        Just coordinate ->
            let
                newPieceCoordinate =
                    movePieceCoordinate coordinate vector
            in
            { board
                | pieceCoordinates = Dict.insert pieceIndex newPieceCoordinate board.pieceCoordinates
                , coordinatePieces =
                    Dict.insert (coordinateToTuple newPieceCoordinate) pieceIndex (Dict.remove (coordinateToTuple coordinate) board.coordinatePieces)
            }

        Nothing ->
            board


coordinateOnMap : BoardCoordinate -> Board -> Bool
coordinateOnMap coordinate board =
    coordinate.x >= 0 && coordinate.x < toFloat board.config.gridDimensions && coordinate.y >= 0 && coordinate.y < toFloat board.config.gridDimensions


movePiece : Board -> Int -> IntVector -> Board
movePiece board pieceIndex vector =
    let
        pieceCoordinate =
            Dict.get pieceIndex board.pieceCoordinates
    in
    case pieceCoordinate of
        Just coordinate ->
            let
                log =
                    Debug.log ("Moving piece " ++ toString pieceIndex ++ " from " ++ toString coordinate ++ " in direction " ++ toString vector) 1

                newCoordinate =
                    getMaxMovementCoordinate pieceIndex board coordinate vector vector

                newCoordinateOnMap =
                    coordinateOnMap newCoordinate board
            in
            if newCoordinateOnMap then
                { board
                    | pieceCoordinates = Dict.insert pieceIndex newCoordinate board.pieceCoordinates
                    , coordinatePieces =
                        Dict.insert (coordinateToTuple newCoordinate) pieceIndex (Dict.remove (coordinateToTuple coordinate) board.coordinatePieces)
                }

            else
                { board
                    | pieceCoordinates = Dict.remove pieceIndex board.pieceCoordinates
                    , coordinatePieces =
                        Dict.remove (coordinateToTuple coordinate) board.coordinatePieces
                    , pieces = Dict.remove pieceIndex board.pieces
                    , pieceVelocities = Dict.remove pieceIndex board.pieceVelocities
                }

        Nothing ->
            board



{-------------------------------------------------
   Score Functions
-------------------------------------------------}


getFieldScore : MagneticField -> Dict Int Float
getFieldScore field =
    let
        totalFieldStrength =
            Dict.foldl (\player strength total -> total + strength) 0 field.playerStrength
    in
    field.playerStrength
        |> Dict.map (\player strength -> strength / totalFieldStrength)


getBoardScores : Board -> Dict Int Float
getBoardScores board =
    let
        playerDict =
            Dict.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]
    in
    Dict.foldl
        (\coordinate field existing ->
            addDicts existing (getFieldScore field)
        )
        playerDict
        board.magneticField



{-------------------------------------------------
   AI Specific Functions 
--------------------------------------------------}


type alias AIMove =
    { x : Int
    , y : Int
    , polarity : Polarity
    }


getFreeCoordinates : Board -> List BoardCoordinate
getFreeCoordinates board =
    let
        currentTaken =
            Dict.values board.pieceCoordinates

        allCoordinates =
            List.range 0 (board.config.gridDimensions - 1)
                |> List.map (\x -> List.range 0 (board.config.gridDimensions - 1) |> List.map (\y -> { x = toFloat x, y = toFloat y }))
                |> List.concat
    in
    List.filter (\coordinate -> not (List.member coordinate currentTaken)) allCoordinates


isSpaceFree : Board -> BoardCoordinate -> Bool
isSpaceFree board coordinate =
    case Dict.get (coordinateToTuple coordinate) board.coordinatePieces of
        Just _ ->
            False

        Nothing ->
            True



{-
   Cell Grid Specific View Functions
-}
{-------------------------------------------------
   View Helper Functions
-------------------------------------------------}


determineCellContent : Dict ( Int, Int ) MagneticField -> Board -> IntCoordinate -> CellContent
determineCellContent magneticField board coordinate =
    let
        piece =
            getPieceFromCoordinate board (intCoordinateToFloat coordinate)

        tentativePiece =
            getTentativePieceFromCoordinate board (intCoordinateToFloat coordinate)

        field =
            Dict.get (intCoordinateToTuple coordinate) magneticField
    in
    case piece of
        Just p ->
            case field of
                Just f ->
                    PieceOnField p f

                Nothing ->
                    GridPiece p

        Nothing ->
            case tentativePiece of
                Just p ->
                    case field of
                        Just f ->
                            PieceOnField p f

                        Nothing ->
                            GridPiece p

                Nothing ->
                    case field of
                        Just f ->
                            GridMagneticField f

                        Nothing ->
                            NoContent


getCellGrid : Board -> CellGrid CellContent
getCellGrid board =
    CellGrid.initialize (Dimensions board.config.gridDimensions board.config.gridDimensions) (\i j -> determineCellContent board.magneticField board { x = j, y = i })


boardHtml : Int -> Board -> Html Msg
boardHtml magnetism board =
    Html.map CellGridMessage (MyCellGrid.asHtml { width = board.config.displaySize, height = board.config.displaySize } (cellStyle magnetism board) (getCellGrid board))


getCellColorFromContent : Int -> Int -> CellContent -> Color.Color
getCellColorFromContent totalPieces magnetism content =
    case content of
        GridPiece piece ->
            Color.darkGrey

        NoContent ->
            Color.darkGrey

        GridMagneticField field ->
            getMagneticFieldColor2 totalPieces magnetism field

        PieceOnField piece field ->
            getPieceColor piece


getCellOpacityFromContent : Float -> CellContent -> Float
getCellOpacityFromContent opacity content =
    case content of
        GridPiece piece ->
            opacity

        NoContent ->
            opacity

        GridMagneticField field ->
            1 - (opacity ^ toFloat (Dict.size field.playerStrength))

        PieceOnField _ field ->
            1 - (opacity ^ toFloat (Dict.size field.playerStrength))


getMagneticFieldColor : Int -> MagneticField -> Color.Color
getMagneticFieldColor magnetism field =
    let
        totalStrength =
            Dict.foldl (\player strength total -> total + strength) 0 field.playerStrength
    in
    Dict.foldl
        (\player strength ( p2, s2 ) ->
            if strength > s2 then
                ( player, strength )

            else
                ( p2, s2 )
        )
        ( 1, -1000 )
        field.playerStrength
        |> (\( player, strength ) -> playerColorToHslaBoard (getPlayerColor player) strength totalStrength (toFloat magnetism))
        |> hslaToColor


getMagneticFieldColor2 : Int -> Int -> MagneticField -> Color.Color
getMagneticFieldColor2 totalPieces magnetism field =
    let
        totalStrength =
            Dict.foldl (\player strength total -> total + strength) 0 field.playerStrength
    in
    Dict.map (\player strength -> ( playerColorToHslaBoard2 totalPieces (getPlayerColor player) strength totalStrength (toFloat magnetism), strength / totalStrength )) field.playerStrength
        |> Dict.values
        |> mergeHslas
        |> fromHsla


getPieceColorFromContent : CellContent -> Color.Color
getPieceColorFromContent content =
    case content of
        GridPiece piece ->
            getPieceColor piece

        PieceOnField piece _ ->
            getPieceColor piece

        _ ->
            Color.white


getTextFromContent : CellContent -> String
getTextFromContent content =
    case content of
        GridPiece piece ->
            toPolarityIcon piece.polarity

        NoContent ->
            ""

        GridMagneticField field ->
            ""

        PieceOnField piece field ->
            toPolarityIcon piece.polarity


cellStyle : Int -> Board -> MyCellGrid.CellStyle CellContent
cellStyle magnetism board =
    { toCellColor = \z -> getCellColorFromContent (Dict.size board.pieces + Dict.size board.tentativePieces) magnetism z
    , toPieceColor = \z -> getPieceColorFromContent z
    , toCellOpacity = \z -> getCellOpacityFromContent 0.7 z
    , toText = \content -> getTextFromContent content
    , cellWidth = toFloat (board.config.displaySize // board.config.gridDimensions)
    , cellHeight = toFloat (board.config.displaySize // board.config.gridDimensions)
    , gridLineColor = Color.rgb 0 0 0
    , gridLineWidth = 0 --toFloat (board.config.displaySize // board.config.gridDimensions) / 20
    , pieceLineWidth = toFloat (board.config.displaySize // board.config.gridDimensions) / 10
    , shouldRenderPiece =
        \content ->
            case content of
                GridPiece _ ->
                    True

                PieceOnField _ _ ->
                    True

                _ ->
                    False
    }
