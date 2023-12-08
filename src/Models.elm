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
    , coordinatePieces : Dict ( Int, Int ) Int
    , magneticField : Dict ( Int, Int ) MagneticField
    , config : BoardConfig
    , tentativePieces : Dict Int Piece
    , tentativePieceCoordinates : Dict Int BoardCoordinate
    , tentativeCoordinatePieces : Dict ( Int, Int ) Int
    }


type alias FloatCoordinate =
    { x : Float
    , y : Float
    }


type alias BoardCoordinate =
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
            { hue = hueToRadians 120, saturation = 1, lightness = 0.6 - (strength / gameMagnetism / 5), alpha = strength / totalStrength }

        Yellow ->
            { hue = hueToRadians 60, saturation = 1, lightness = 0.6 - (strength / gameMagnetism / 5), alpha = strength / totalStrength }

        White ->
            { hue = hueToRadians 0, saturation = 1, lightness = 1, alpha = 0 }


getLigthness : Float -> Float -> Float
getLigthness strength gameMagnetism =
    strength / gameMagnetism / 1.2 - 0.1


hueToRadians : Float -> Float
hueToRadians hue =
    hue / 360


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


toTuple : BoardCoordinate -> ( Int, Int )
toTuple coordinate =
    ( coordinate.x, coordinate.y )


fromTuple : ( Int, Int ) -> BoardCoordinate
fromTuple ( x, y ) =
    { x = x, y = y }


fromArgs : Int -> Int -> BoardCoordinate
fromArgs x y =
    { x = x, y = y }


coordinateToString : BoardCoordinate -> String
coordinateToString coordinate =
    "(" ++ String.fromInt coordinate.x ++ "," ++ String.fromInt coordinate.y ++ ")"


euclideanDistance : BoardCoordinate -> BoardCoordinate -> Float
euclideanDistance coordinate1 coordinate2 =
    let
        xDiff =
            toFloat (coordinate1.x - coordinate2.x)

        yDiff =
            toFloat (coordinate1.y - coordinate2.y)
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
    }


getPieceFromCoordinate : Board -> BoardCoordinate -> Maybe Piece
getPieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.coordinatePieces
                |> Dict.get (toTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.pieces) pieceIndex


getTentativePieceFromCoordinate : Board -> BoardCoordinate -> Maybe Piece
getTentativePieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.tentativeCoordinatePieces
                |> Dict.get (toTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.tentativePieces) pieceIndex



{-------------------------------------------------
   Magnetic Field Calculation Functions
-------------------------------------------------}


mergeMagneticFields : MagneticField -> MagneticField -> MagneticField
mergeMagneticFields field1 field2 =
    { positiveVector = { x = field1.positiveVector.x + field2.positiveVector.x, y = field1.positiveVector.y + field2.positiveVector.y }
    , negativeVector = { x = field1.negativeVector.x + field2.negativeVector.x, y = field1.negativeVector.y + field2.negativeVector.y }
    , playerStrength = addDicts field1.playerStrength field2.playerStrength
    }


updateMagneticFieldDict : List ( BoardCoordinate, MagneticField ) -> Dict ( Int, Int ) MagneticField -> Dict ( Int, Int ) MagneticField
updateMagneticFieldDict fields magneticFields =
    case fields of
        [] ->
            magneticFields

        ( coordinate, field ) :: rest ->
            let
                currentField =
                    Dict.get (toTuple coordinate) magneticFields
            in
            case currentField of
                Just f ->
                    let
                        newField =
                            mergeMagneticFields f field
                    in
                    updateMagneticFieldDict rest (Dict.insert (toTuple coordinate) newField magneticFields)

                Nothing ->
                    updateMagneticFieldDict rest (Dict.insert (toTuple coordinate) field magneticFields)


getBoardMagneticField : Board -> Int -> Dict ( Int, Int ) MagneticField
getBoardMagneticField board magnetism =
    let
        allCoordinates =
            List.range 0 (board.config.gridDimensions - 1)
                |> List.map (\x -> List.range 0 (board.config.gridDimensions - 1) |> List.map (\y -> { x = x, y = y }))
                |> List.concat
    in
    getBoardMagneticFieldRec board magnetism allCoordinates Dict.empty


getBoardMagneticFieldRec : Board -> Int -> List BoardCoordinate -> Dict ( Int, Int ) MagneticField -> Dict ( Int, Int ) MagneticField
getBoardMagneticFieldRec board magnetism remainingCoordinates magneticFields =
    case remainingCoordinates of
        [] ->
            magneticFields

        coordinate :: rest ->
            let
                currentField =
                    getMagneticFieldForCoordinate board magnetism coordinate
            in
            getBoardMagneticFieldRec board magnetism rest (Dict.insert (toTuple coordinate) currentField magneticFields)


getCoordinatePieces : Board -> List ( BoardCoordinate, Piece )
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
                        Just ( fromTuple coordinate, piece )

                    Nothing ->
                        Nothing
            )


getMagneticFieldForCoordinate : Board -> Int -> BoardCoordinate -> MagneticField
getMagneticFieldForCoordinate board magnetism coordinate =
    let
        coordinatePieces =
            getCoordinatePieces board
    in
    List.foldl
        (\( coord, piece ) existingFields ->
            mergeMagneticFields existingFields (getFieldFromPieceAtCoordinate magnetism coordinate coord piece)
        )
        emptyMagneticField
        coordinatePieces


getFieldFromPieceAtCoordinate : Int -> BoardCoordinate -> BoardCoordinate -> Piece -> MagneticField
getFieldFromPieceAtCoordinate magnetism coordinate pieceCoordinate piece =
    if coordinate == pieceCoordinate then
        emptyMagneticField

    else
        let
            eDistance =
                euclideanDistance coordinate pieceCoordinate

            distanceVector =
                { x = toFloat (coordinate.x - pieceCoordinate.x), y = toFloat (coordinate.y - pieceCoordinate.y) }

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


updatePiecePositions : Int -> Board -> Board
updatePiecePositions magnitude board =
    let
        coordinatePieces =
            Dict.values (Dict.map (\coordinate index -> ( coordinate, index, Dict.get index board.pieces )) board.coordinatePieces)
    in
    updatePiecePositionsRecursive magnitude coordinatePieces board.magneticField board


updatePiecePositionsRecursive : Int -> List ( ( Int, Int ), Int, Maybe Piece ) -> Dict ( Int, Int ) MagneticField -> Board -> Board
updatePiecePositionsRecursive magnitude coordinatePieces magneticFields board =
    case coordinatePieces of
        [] ->
            board

        coordinatePiece :: rest ->
            let
                ( coordinate, pieceIndex, piece ) =
                    coordinatePiece

                field =
                    Dict.get coordinate magneticFields
            in
            case piece of
                Just p ->
                    case field of
                        Just f ->
                            let
                                movementVector =
                                    scaledIntUnit magnitude (getMovementVectorForMagnet 1 (toFloat magnitude) f p)
                            in
                            updatePiecePositionsRecursive magnitude rest magneticFields (movePiece board pieceIndex movementVector)

                        Nothing ->
                            updatePiecePositionsRecursive magnitude rest magneticFields board

                Nothing ->
                    updatePiecePositionsRecursive magnitude rest magneticFields board


getMovementVectorForMagnet : Float -> Float -> MagneticField -> Piece -> IntVector
getMovementVectorForMagnet threshold magnitude field piece =
    let
        resultantVector =
            case piece.polarity of
                Positive ->
                    multiplyVector magnitude (combineVectors field.positiveVector (negative field.negativeVector))

                Negative ->
                    multiplyVector magnitude (combineVectors field.negativeVector (negative field.positiveVector))

                None ->
                    { x = 0, y = 0 }

        xMagnitude =
            abs resultantVector.x

        yMagnitude =
            abs resultantVector.y

        resultantX =
            if xMagnitude > threshold then
                round resultantVector.x

            else
                0

        resultantY =
            if yMagnitude > threshold then
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
        , coordinatePieces = Dict.insert (toTuple coordinate) pieceIndex board.coordinatePieces
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
        , tentativeCoordinatePieces = Dict.fromList [ ( toTuple coordinate, pieceIndex ) ]
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
    { x = coordinate.x + vector.x, y = coordinate.y + vector.y }


getMaxMovementCoordinate : Int -> Board -> BoardCoordinate -> IntVector -> BoardCoordinate
getMaxMovementCoordinate pieceIndex board coordinate vector =
    let
        unitVector =
            unitIntVector vector

        tentativeNext =
            movePieceCoordinate coordinate (unitIntVector vector)

        tentativeNextPiece =
            getPieceFromCoordinate board tentativeNext
    in
    case tentativeNextPiece of
        Just _ ->
            coordinate

        Nothing ->
            getMaxMovementCoordinate pieceIndex (movePieceUnsafe board pieceIndex unitVector) tentativeNext (decreaseIntVectorMagnitude vector)


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
                    movePieceCoordinate vector coordinate
            in
            { board
                | pieceCoordinates = Dict.insert pieceIndex newPieceCoordinate board.pieceCoordinates
                , coordinatePieces =
                    Dict.insert (toTuple newPieceCoordinate) pieceIndex (Dict.remove (toTuple coordinate) board.coordinatePieces)
            }

        Nothing ->
            board


coordinateOnMap : BoardCoordinate -> Board -> Bool
coordinateOnMap coordinate board =
    coordinate.x >= 0 && coordinate.x < board.config.gridDimensions && coordinate.y >= 0 && coordinate.y < board.config.gridDimensions


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
                    coordinateOnMap newCoordinate board
            in
            if newCoordinateOnMap then
                { board
                    | pieceCoordinates = Dict.insert pieceIndex newCoordinate board.pieceCoordinates
                    , coordinatePieces =
                        Dict.insert (toTuple newCoordinate) pieceIndex (Dict.remove (toTuple coordinate) board.coordinatePieces)
                }

            else
                { board
                    | pieceCoordinates = Dict.remove pieceIndex board.pieceCoordinates
                    , coordinatePieces =
                        Dict.remove (toTuple coordinate) board.coordinatePieces
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
            if coordinateOnMap (fromTuple coordinate) board then
                addDicts existing (getFieldScore field)

            else
                existing
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
                |> List.map (\x -> List.range 0 (board.config.gridDimensions - 1) |> List.map (\y -> { x = x, y = y }))
                |> List.concat
    in
    List.filter (\coordinate -> not (List.member coordinate currentTaken)) allCoordinates


isSpaceFree : Board -> BoardCoordinate -> Bool
isSpaceFree board coordinate =
    case Dict.get (toTuple coordinate) board.coordinatePieces of
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


determineCellContent : Dict ( Int, Int ) MagneticField -> Board -> BoardCoordinate -> CellContent
determineCellContent magneticField board coordinate =
    let
        piece =
            getPieceFromCoordinate board coordinate

        tentativePiece =
            getTentativePieceFromCoordinate board coordinate

        field =
            Dict.get (toTuple coordinate) magneticField
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
    CellGrid.initialize (Dimensions board.config.gridDimensions board.config.gridDimensions) (\i j -> determineCellContent board.magneticField board (fromArgs j i))


boardHtml : Int -> Board -> Html Msg
boardHtml magnetism board =
    Html.map CellGridMessage (MyCellGrid.asHtml { width = board.config.displaySize, height = board.config.displaySize } (cellStyle magnetism board) (getCellGrid board))


getCellColorFromContent : Int -> CellContent -> Color.Color
getCellColorFromContent magnetism content =
    case content of
        GridPiece piece ->
            Color.darkGrey

        NoContent ->
            Color.darkGrey

        GridMagneticField field ->
            getMagneticFieldColor magnetism field

        PieceOnField _ field ->
            getMagneticFieldColor magnetism field


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


getPieceColorFromContent : CellContent -> Color.Color
getPieceColorFromContent content =
    case content of
        GridPiece piece ->
            getPieceColor piece

        PieceOnField piece _ ->
            getPieceColor piece

        _ ->
            Color.white


getMergedCellColor : List PlayerColor -> Color.Color
getMergedCellColor players =
    case players of
        [] ->
            Color.white

        playerColors ->
            blendRGB (List.map playerColorToRGB playerColors)
                |> colorFromTuple


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
    { toCellColor = \z -> getCellColorFromContent magnetism z
    , toPieceColor = \z -> getPieceColorFromContent z
    , toCellOpacity = \z -> getCellOpacityFromContent 0.7 z
    , toText = \content -> getTextFromContent content
    , cellWidth = toFloat (board.config.displaySize // board.config.gridDimensions)
    , cellHeight = toFloat (board.config.displaySize // board.config.gridDimensions)
    , gridLineColor = Color.rgb 0 0 0
    , gridLineWidth = 0 --toFloat (board.config.displaySize // board.config.gridDimensions) / 20
    , pieceLineWidth = toFloat (board.config.displaySize // board.config.gridDimensions) / 5
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
