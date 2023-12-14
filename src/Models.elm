module Models exposing (..)

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
import Utils.ColorUtils exposing (..)
import Utils.PhysicsUtils exposing (..)
import Utils.Utils exposing (..)
import Utils.VectorUtils exposing (..)



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
    , deadPieces : Dict ( Int, Int ) Piece
    , padding : Int
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
    , padding : Int
    }


type CellContent
    = GridPiece Piece
    | NoContent
    | GridMagneticField MagneticField
    | PieceOnField Piece MagneticField
    | DarkZone (Maybe Piece) Int



{-------------------------------------------------
   Utility Functions
-------------------------------------------------}


emptyHsla : Hsla
emptyHsla =
    { hue = 0, saturation = 0, lightness = 0, alpha = 0 }


playerColorToHslaBoard : Int -> PlayerColor -> Float -> Float -> Hsla
playerColorToHslaBoard totalPieces playerColor strength gameMagnetism =
    case playerColor of
        Red ->
            { hue = hueToRadians 0, saturation = 1, lightness = 0.65 - (0.5 * Basics.sqrt (strength / toFloat totalPieces / gameMagnetism)), alpha = 0.9 }

        Blue ->
            { hue = hueToRadians 240, saturation = 1, lightness = 0.65 - (0.5 * Basics.sqrt (strength / toFloat totalPieces / gameMagnetism)), alpha = 0.9 }

        Green ->
            { hue = hueToRadians 120, saturation = 1, lightness = 0.65 - (0.5 * (strength / toFloat totalPieces / gameMagnetism)), alpha = 0.9 }

        Yellow ->
            { hue = hueToRadians 60, saturation = 1, lightness = 0.65 - (0.5 * (strength / toFloat totalPieces / gameMagnetism)), alpha = 0.9 }

        White ->
            { hue = hueToRadians 0, saturation = 1, lightness = 1, alpha = 0 }


hslaToColor : Hsla -> Color.Color
hslaToColor hsla =
    fromHsla hsla


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
    playerColorToHslaBoard 1 (getPlayerColor piece.player) 1 1
        |> (\hsla -> { hsla | lightness = lightness, alpha = 1 })
        |> hslaToColor


type Polarity
    = Positive
    | Negative
    | None


toPolarityIcon : Polarity -> String
toPolarityIcon polarity =
    case polarity of
        Positive ->
            "+"

        Negative ->
            "−"

        None ->
            ""


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
    , deadPieces = Dict.empty
    , padding = boardConfig.padding
    }


{-| Gets a piece from a given coordinate if it exists
-}
getPieceFromCoordinate : Board -> BoardCoordinate -> Maybe Piece
getPieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.coordinatePieces
                |> Dict.get (coordinateToTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.pieces) pieceIndex


{-| Gets a tentative piece from a given coordinate if it exists
-}
getTentativePieceFromCoordinate : Board -> BoardCoordinate -> Maybe Piece
getTentativePieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.tentativeCoordinatePieces
                |> Dict.get (coordinateToTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.tentativePieces) pieceIndex


{-| Gets a dead piece from a given coordinate if it exists
-}
getDeadPieceFromCoordinate : Board -> IntCoordinate -> Maybe Piece
getDeadPieceFromCoordinate board coordinate =
    Dict.get (intCoordinateToTuple coordinate) board.deadPieces


{-| Checks if a given coordinate can be occupied by a piece
-}
checkValidPiecePlacement : BoardCoordinate -> Maybe BoardCoordinate -> Board -> Bool
checkValidPiecePlacement coordinate toOmit board =
    let
        allCheckedCoordinates =
            getSurroundingCoordinates coordinate

        checkedCoordinates =
            case toOmit of
                Just omit ->
                    allCheckedCoordinates
                        |> List.filter (\coord -> coord /= omit)

                Nothing ->
                    allCheckedCoordinates
    in
    List.filterMap (\coord -> getPieceFromCoordinate board coord) checkedCoordinates
        |> List.isEmpty


{-| Utility function to check if a coordinate exists on the board
-}
coordinateOnBoard : BoardCoordinate -> Board -> Bool
coordinateOnBoard coordinate board =
    coordinate.x >= 0 && coordinate.x < toFloat board.config.gridDimensions && coordinate.y >= 0 && coordinate.y < toFloat board.config.gridDimensions


{-| Adds 2 dictionaries of vectors together
-}
addForceDicts : Dict Int FloatVector -> Dict Int FloatVector -> Dict Int FloatVector
addForceDicts forceDict newForceDict =
    Dict.foldl (\player force accumDict -> Dict.insert player (combineMaybeVectors force (Dict.get player accumDict)) accumDict) forceDict newForceDict


{-| Gets a mapping of pieceIndex, coordinate, and piece
-}
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
                Maybe.map (\piece -> ( pieceIndex, coordinateFromTuple coordinate, piece )) (Dict.get pieceIndex allPieces)
            )



{-------------------------------------------------
   Magnetic Field Calculation Functions
-------------------------------------------------}


{-| Recalculates and updates the magnetic field on the board
-}
updateBoardMagneticField : Int -> Board -> Board
updateBoardMagneticField magnetism board =
    { board
        | magneticField = getBoardMagneticField board magnetism
    }


{-| Recalculates the Magnetic Field on the board
-}
getBoardMagneticField : Board -> Int -> Dict ( Int, Int ) MagneticField
getBoardMagneticField board magnetism =
    let
        allCoordinates =
            List.range 0 (board.config.gridDimensions - 1)
                |> List.map (\x -> List.range 0 (board.config.gridDimensions - 1) |> List.map (\y -> { x = x, y = y }))
                |> List.concat
    in
    getBoardMagneticFieldRec board magnetism allCoordinates Dict.empty


{-| Helper function to calculate magnetic field recursively
-}
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


{-| Gets Magnetic field for at a specific Coordinate
-}
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


{-| Calculates the magnetic field exerted by a single piece on a single coordinate
-}
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


{-| Utility Function to merge 2 magnetic fields at a point
-}
mergeMagneticFields : MagneticField -> MagneticField -> MagneticField
mergeMagneticFields field1 field2 =
    { positiveVector = { x = field1.positiveVector.x + field2.positiveVector.x, y = field1.positiveVector.y + field2.positiveVector.y }
    , negativeVector = { x = field1.negativeVector.x + field2.negativeVector.x, y = field1.negativeVector.y + field2.negativeVector.y }
    , playerStrength = addDicts field1.playerStrength field2.playerStrength
    }



{-------------------------------------------------
   Velocity Update Functions
-------------------------------------------------}


{-| Main function to update the board state with velocities of pieces based on friction and magnetism
-}
updatePieceVelocities : Float -> Int -> Board -> Board
updatePieceVelocities friction magnetism board =
    let
        magneticForceDict =
            getMagneticForceOnAllPieces magnetism board

        newVelocities =
            addForceDicts board.pieceVelocities magneticForceDict

        newVelocitiesWithFriction =
            Dict.map
                (\index vector -> combineVectors (getFrictionVector friction vector) vector)
                newVelocities
    in
    { board
        | pieceVelocities = newVelocitiesWithFriction
    }
        |> updatePieceCollisions
        |> checkAllVectorStop


{-| Calculates the magnetic force enacted on each piece at a specific moment
-}
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
            addForceDicts accumDict resultantDict
        )
        Dict.empty
        coordinatePairs


{-| Utility function to calculate the magnetic force between 2 pieces
-}
getMagneticForceBetweenPieces : Int -> BoardCoordinate -> Piece -> BoardCoordinate -> Piece -> FloatVector
getMagneticForceBetweenPieces magnetism coordinate1 piece1 coordinate2 piece2 =
    let
        ( force, totalStrength ) =
            calculateForceVector magnetism coordinate1 coordinate2
    in
    if piece1.polarity == piece2.polarity then
        force

    else
        negative force


{-| Calculates board Velocities after Collisions
-}
updatePieceCollisions : Board -> Board
updatePieceCollisions board =
    let
        updates =
            getCollidingPieceUpdates board board.pieceVelocities

        newPieceVelocities =
            addForceDicts board.pieceVelocities updates
    in
    { board
        | pieceVelocities = newPieceVelocities
    }


{-| Zeroes vectors which have hit a specific threshold
-}
checkAllVectorStop : Board -> Board
checkAllVectorStop board =
    let
        newPieceVelocities =
            Dict.map (\i v -> checkVectorStop v) board.pieceVelocities
    in
    { board
        | pieceVelocities = newPieceVelocities
    }


{-| Zeroes all piece velocities between moves
-}
zeroPieceVelocities : Board -> Board
zeroPieceVelocities board =
    { board
        | pieceVelocities = Dict.map (\i v -> { x = 0, y = 0 }) board.pieceVelocities
    }


{-| Gets deceleration of colliding pieces
-}
getCollidingPieceUpdates : Board -> Dict Int FloatVector -> Dict Int FloatVector
getCollidingPieceUpdates board velocities =
    let
        collidingPieces =
            getCollidingPieces board

        velocitiesAfterCollision =
            List.foldl
                (\( ( i1, c1, _ ), ( i2, c2, _ ) ) accumDict ->
                    case ( Dict.get i1 velocities, Dict.get i2 velocities ) of
                        ( Just v1, Just v2 ) ->
                            let
                                ( antiV1, antiV2 ) =
                                    calculateCollisionVector ( c1, v1 ) ( c2, v2 )
                            in
                            addForceDicts (Dict.fromList [ ( i1, antiV1 ), ( i2, antiV2 ) ]) accumDict

                        _ ->
                            accumDict
                )
                Dict.empty
                collidingPieces
    in
    velocitiesAfterCollision


{-| Checks piece positions to get colliding pieces
-}
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



{-------------------------------------------------
   Piece Movement and Insertion Functions
-------------------------------------------------}


{-| Inserts a piece at a specific coordinate on the board
-}
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


{-| Inserts a tentative piece at a specific coordinate on the board for display purposes
-}
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


{-| Empties out displayed tentative pieces on the board
-}
removeTentativePieces : Board -> Board
removeTentativePieces board =
    { board
        | tentativePieceCoordinates = Dict.empty
        , tentativeCoordinatePieces = Dict.empty
        , tentativePieces = Dict.empty
    }


{-| Main function to update all piece positions on the board after each update
-}
updatePiecePositions : Board -> Board
updatePiecePositions board =
    let
        coordinatePieces =
            Dict.values (Dict.map (\coordinate index -> ( coordinate, index, Dict.get index board.pieces )) board.coordinatePieces)
    in
    updatePiecePositionsRecursive coordinatePieces board


{-| Helper function to update piece positions recursively
-}
updatePiecePositionsRecursive : List ( ( Float, Float ), Int, Maybe Piece ) -> Board -> Board
updatePiecePositionsRecursive coordinatePieces board =
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
                                    floatCoordinateToInt (multiplyVector 1 f)
                            in
                            updatePiecePositionsRecursive rest (movePiece board pieceIndex movementVector)

                        Nothing ->
                            updatePiecePositionsRecursive rest board

                Nothing ->
                    updatePiecePositionsRecursive rest board


{-| Updates a piece position on the board with some movement vector
-}
movePiece : Board -> Int -> IntVector -> Board
movePiece board pieceIndex vector =
    let
        pieceCoordinate =
            Dict.get pieceIndex board.pieceCoordinates
    in
    case pieceCoordinate of
        Just coordinate ->
            let
                newCoordinate =
                    getMaxMovementCoordinate pieceIndex board coordinate vector

                newCoordinateOnMap =
                    coordinateOnBoard newCoordinate board
            in
            if newCoordinateOnMap then
                { board
                    | pieceCoordinates = Dict.insert pieceIndex newCoordinate board.pieceCoordinates
                    , coordinatePieces =
                        Dict.insert (coordinateToTuple newCoordinate) pieceIndex (Dict.remove (coordinateToTuple coordinate) board.coordinatePieces)
                }

            else
                case Dict.get pieceIndex board.pieces of
                    Just piece ->
                        { board
                            | pieceCoordinates = Dict.remove pieceIndex board.pieceCoordinates
                            , coordinatePieces =
                                Dict.remove (coordinateToTuple coordinate) board.coordinatePieces
                            , pieces = Dict.remove pieceIndex board.pieces
                            , pieceVelocities = Dict.remove pieceIndex board.pieceVelocities
                            , deadPieces = Dict.insert (intCoordinateToTuple (getNearestViewCoordinateToDeadPiece board newCoordinate)) piece board.deadPieces
                        }

                    Nothing ->
                        board

        Nothing ->
            board


{-| Maps a dead piece's coordinate to a border square
-}
getNearestViewCoordinateToDeadPiece : Board -> BoardCoordinate -> IntCoordinate
getNearestViewCoordinateToDeadPiece board coordinate =
    let
        padding =
            board.padding

        isRight =
            coordinate.x >= toFloat board.config.gridDimensions

        isLeft =
            coordinate.x < 0

        isBottom =
            coordinate.y >= toFloat board.config.gridDimensions

        isTop =
            coordinate.y < 0

        nearestX =
            if isRight then
                board.config.gridDimensions + padding + (padding // 2)

            else if isLeft then
                padding - (padding // 2 + 1)

            else
                Basics.round coordinate.x + padding

        nearestY =
            if isBottom then
                board.config.gridDimensions + padding + (padding // 2)

            else if isTop then
                padding - (padding // 2 + 1)

            else
                Basics.round coordinate.y + padding

        taken =
            getDeadPieceFromCoordinate board { x = nearestX, y = nearestY }
    in
    case taken of
        Nothing ->
            { x = nearestX, y = nearestY }

        Just _ ->
            if isRight then
                { x = nearestX - 1, y = nearestY }

            else if isLeft then
                { x = nearestX + 1, y = nearestY }

            else if isBottom then
                { x = nearestX, y = nearestY - 1 }

            else if isTop then
                { x = nearestX, y = nearestY + 1 }

            else
                { x = nearestX, y = nearestY }


{-| Checks path of piece movement in a specific direction to determine limit of piece movement
-}
getMaxMovementCoordinate : Int -> Board -> BoardCoordinate -> IntVector -> BoardCoordinate
getMaxMovementCoordinate pieceIndex board coordinate vector =
    let
        unitVector =
            unitIntVector vector

        actualNext =
            movePieceCoordinate coordinate unitVector

        isNextSpaceValid =
            checkValidPiecePlacement actualNext (Just coordinate) board
    in
    if coordinate == actualNext then
        coordinate

    else if isNextSpaceValid then
        getMaxMovementCoordinate pieceIndex (movePieceUnsafe board pieceIndex unitVector) actualNext (decreaseIntVectorMagnitude vector)

    else
        coordinate


{-| Utility function to simulate update the position of a piece: Only use for simulation: Does not check for collision
-}
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



{-------------------------------------------------
   Score Functions
-------------------------------------------------}


{-| Gets the total score for each player based on the board state
-}
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


{-| Calculates the scores for each player for a single space based on it's magnetic field
-}
getFieldScore : MagneticField -> Dict Int Float
getFieldScore field =
    let
        totalFieldStrength =
            Dict.foldl (\player strength total -> total + strength) 0 field.playerStrength
    in
    field.playerStrength
        |> Dict.map (\player strength -> strength / totalFieldStrength)



{-------------------------------------------------
   AI Specific Functions 
--------------------------------------------------}


type alias AIMove =
    { x : Int
    , y : Int
    , polarity : Polarity
    }


{-| Gets a list of available coordinates on the board
-}
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


{-| Checks to see if a space on the board is free
-}
isSpaceFree : Board -> BoardCoordinate -> Bool
isSpaceFree board coordinate =
    case Dict.get (coordinateToTuple coordinate) board.coordinatePieces of
        Just _ ->
            False

        Nothing ->
            True



{-------------------------------------------------
   View Helper Functions
-------------------------------------------------}


determineCellContent : Int -> Dict ( Int, Int ) MagneticField -> Board -> IntCoordinate -> CellContent
determineCellContent padding magneticField board coordinate =
    let
        boardCoordinates =
            mapViewCoordinateToBoard padding board.config.gridDimensions coordinate
    in
    case boardCoordinates of
        Just boardCoord ->
            let
                piece =
                    getPieceFromCoordinate board (intCoordinateToFloat boardCoord)

                tentativePiece =
                    getTentativePieceFromCoordinate board (intCoordinateToFloat boardCoord)

                field =
                    Dict.get (intCoordinateToTuple boardCoord) magneticField
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

        Nothing ->
            case getDeadPieceFromCoordinate board coordinate of
                Just p ->
                    DarkZone
                        (Just p)
                        (getDarkZoneDegree padding board coordinate)

                Nothing ->
                    DarkZone Nothing (getDarkZoneDegree padding board coordinate)


getDarkZoneDegree : Int -> Board -> IntCoordinate -> Int
getDarkZoneDegree padding board coordinate =
    let
        xDegree =
            if coordinate.x < padding then
                abs (padding - coordinate.x)

            else if coordinate.x >= board.config.gridDimensions + padding then
                abs (coordinate.x - (board.config.gridDimensions + padding - 1))

            else
                0

        yDegree =
            if coordinate.y < padding then
                abs (padding - coordinate.y)

            else if coordinate.y >= board.config.gridDimensions + padding then
                abs (coordinate.y - (board.config.gridDimensions + padding - 1))

            else
                0
    in
    Basics.max xDegree yDegree


getCellGrid : Int -> Board -> CellGrid CellContent
getCellGrid padding board =
    let
        gridDimensions =
            board.config.gridDimensions + padding * 2
    in
    CellGrid.initialize (Dimensions gridDimensions gridDimensions) (\i j -> determineCellContent padding board.magneticField board { x = j, y = i })


boardHtml : Int -> Int -> Board -> Html Msg
boardHtml padding magnetism board =
    Html.map CellGridMessage (MyCellGrid.asHtml { width = board.config.displaySize, height = board.config.displaySize } (cellStyle padding magnetism board) (getCellGrid padding board))


getCellColorFromContent : Int -> Int -> CellContent -> Color.Color
getCellColorFromContent totalPieces magnetism content =
    case content of
        GridPiece piece ->
            Color.darkGrey

        NoContent ->
            Color.darkGrey

        GridMagneticField field ->
            getMagneticFieldColorMixed totalPieces magnetism field

        PieceOnField piece field ->
            getPieceColor piece

        DarkZone _ 1 ->
            Color.fromRgba { red = 0, blue = 0, green = 0, alpha = 0.3 }

        DarkZone _ 2 ->
            Color.fromRgba { red = 0, blue = 0, green = 0, alpha = 0.1 }

        DarkZone _ _ ->
            Color.white


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

        DarkZone _ _ ->
            1


getMagneticFieldColorWinner : Int -> Int -> MagneticField -> Color.Color
getMagneticFieldColorWinner totalPieces magnetism field =
    Dict.foldl
        (\player strength ( p2, s2 ) ->
            if strength > s2 then
                ( player, strength )

            else
                ( p2, s2 )
        )
        ( 1, -1000 )
        field.playerStrength
        |> (\( player, strength ) -> playerColorToHslaBoard totalPieces (getPlayerColor player) strength (toFloat magnetism))
        |> hslaToColor


getMagneticFieldColorMixed : Int -> Int -> MagneticField -> Color.Color
getMagneticFieldColorMixed totalPieces magnetism field =
    let
        totalStrength =
            Dict.foldl (\player strength total -> total + strength) 0 field.playerStrength
    in
    if totalStrength > 0 then
        Dict.map (\player strength -> ( playerColorToHslaBoard totalPieces (getPlayerColor player) strength (toFloat magnetism), strength / totalStrength )) field.playerStrength
            |> Dict.values
            |> mergeHslas
            |> fromHsla

    else
        Color.darkGrey


getPieceColorFromContent : CellContent -> Color.Color
getPieceColorFromContent content =
    case content of
        GridPiece piece ->
            getPieceColor piece

        PieceOnField piece _ ->
            getPieceColor piece

        DarkZone (Just piece) _ ->
            getPieceColor piece

        _ ->
            Color.white


getPieceScaleFromContent : CellContent -> Float
getPieceScaleFromContent content =
    case content of
        GridPiece piece ->
            1.0

        PieceOnField piece _ ->
            1.0

        DarkZone (Just piece) _ ->
            0.65

        _ ->
            1.0


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

        DarkZone (Just p) _ ->
            toPolarityIcon p.polarity

        DarkZone Nothing _ ->
            ""


getTextColorFromContent : CellContent -> Color
getTextColorFromContent content =
    let
        textColorFromPiece piece =
            case piece.polarity of
                Positive ->
                    Color.darkCharcoal

                Negative ->
                    Color.darkGray

                None ->
                    Color.darkCharcoal
    in
    case content of
        GridPiece piece ->
            textColorFromPiece piece

        NoContent ->
            Color.black

        GridMagneticField _ ->
            Color.black

        PieceOnField piece _ ->
            textColorFromPiece piece

        DarkZone (Just p) _ ->
            textColorFromPiece p

        DarkZone Nothing _ ->
            Color.black


mapViewCoordinateToBoard : Int -> Int -> IntCoordinate -> Maybe IntCoordinate
mapViewCoordinateToBoard padding boardSize viewCoordinate =
    let
        newX =
            viewCoordinate.x - padding

        newY =
            viewCoordinate.y - padding
    in
    if newX >= 0 && newY >= 0 && newX < boardSize && newY < boardSize then
        Just { x = newX, y = newY }

    else
        Nothing


mapBoardCoordinateToView : Int -> IntCoordinate -> IntCoordinate
mapBoardCoordinateToView padding coordinate =
    { x = coordinate.x + padding, y = coordinate.y + padding }


cellStyle : Int -> Int -> Board -> MyCellGrid.CellStyle CellContent
cellStyle padding magnetism board =
    { toCellColor = \z -> getCellColorFromContent (Dict.size board.pieces + Dict.size board.tentativePieces) magnetism z
    , toPieceColor = \z -> getPieceColorFromContent z
    , toPieceScale = \z -> getPieceScaleFromContent z
    , toCellOpacity = \z -> getCellOpacityFromContent 0.7 z
    , toText = \content -> getTextFromContent content
    , cellWidth = toFloat (board.config.displaySize // (board.config.gridDimensions + 2 * padding))
    , toPieceTextColor = \content -> getTextColorFromContent content
    , cellHeight = toFloat (board.config.displaySize // (board.config.gridDimensions + 2 * padding))
    , gridLineColor = Color.rgb 0 0 0
    , gridLineWidth = 0
    , pieceLineWidth = toFloat (board.config.displaySize // board.config.gridDimensions) / 10
    , shouldRenderPiece =
        \content ->
            case content of
                GridPiece _ ->
                    True

                PieceOnField _ _ ->
                    True

                DarkZone (Just p) _ ->
                    True

                _ ->
                    False
    }
