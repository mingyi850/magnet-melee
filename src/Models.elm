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
import Physics exposing (..)
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
    playerColorToHslaBoard 1 (getPlayerColor piece.player) 1 1
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


getTentativePieceFromCoordinate : Board -> BoardCoordinate -> Maybe Piece
getTentativePieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.tentativeCoordinatePieces
                |> Dict.get (coordinateToTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.tentativePieces) pieceIndex


coordinateOnBoard : BoardCoordinate -> Board -> Bool
coordinateOnBoard coordinate board =
    coordinate.x >= 0 && coordinate.x < toFloat board.config.gridDimensions && coordinate.y >= 0 && coordinate.y < toFloat board.config.gridDimensions


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


updatePieceCollisions : Board -> Board
updatePieceCollisions board =
    let
        updates =
            getCollidingPieceUpdates board board.pieceVelocities

        newPieceVelocities =
            updateForceDict board.pieceVelocities updates
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


updatePieceVelocities : Float -> Int -> Board -> Board
updatePieceVelocities friction magnetism board =
    let
        magneticForceDict =
            getMagneticForceOnAllPieces magnetism board

        newVelocities =
            updateForceDict board.pieceVelocities magneticForceDict

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


zeroPieceVelocities : Board -> Board
zeroPieceVelocities board =
    { board
        | pieceVelocities = Dict.map (\i v -> { x = 0, y = 0 }) board.pieceVelocities
    }


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
                            updateForceDict (Dict.fromList [ ( i1, antiV1 ), ( i2, antiV2 ) ]) accumDict

                        _ ->
                            accumDict
                )
                Dict.empty
                collidingPieces
    in
    velocitiesAfterCollision


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

    else
        negative force



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


updatePiecePositions : Board -> Board
updatePiecePositions board =
    let
        coordinatePieces =
            Dict.values (Dict.map (\coordinate index -> ( coordinate, index, Dict.get index board.pieces )) board.coordinatePieces)
    in
    updatePiecePositionsRecursive coordinatePieces board


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
            getMagneticFieldColorMixed totalPieces magnetism field

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
    , gridLineWidth = 0
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
