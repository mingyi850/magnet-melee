module Models exposing (..)

import CellGrid exposing (..)
import Color exposing (..)
import Common exposing (Player)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MyCellGrid exposing (..)
import Utils exposing (..)



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
    , players : List Int
    }


type alias Piece =
    { player : Int
    , polarity : Polarity
    }


type alias Board =
    { pieces : Dict Int Piece
    , pieceCoordinates : Dict Int Coordinate
    , coordinatePieces : Dict ( Int, Int ) Int
    , magneticField : Dict ( Int, Int ) MagneticField
    , config : BoardConfig
    , tentativePieces : Dict Int Piece
    , tentativePieceCoordinates : Dict Int Coordinate
    , tentativeCoordinatePieces : Dict ( Int, Int ) Int
    }


type alias Coordinate =
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


type alias FloatVector =
    { x : Float
    , y : Float
    }


type alias IntVector =
    { x : Int
    , y : Int
    }



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


playerColorToColor : PlayerColor -> Color.Color
playerColorToColor color =
    colorFromTuple (playerColorToRGB color)


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


toTuple : Coordinate -> ( Int, Int )
toTuple coordinate =
    ( coordinate.x, coordinate.y )


fromTuple : ( Int, Int ) -> Coordinate
fromTuple ( x, y ) =
    { x = x, y = y }


fromArgs : Int -> Int -> Coordinate
fromArgs x y =
    { x = x, y = y }


toString : Coordinate -> String
toString coordinate =
    "(" ++ String.fromInt coordinate.x ++ "," ++ String.fromInt coordinate.y ++ ")"


negative : FloatVector -> FloatVector
negative vector =
    { x = -vector.x, y = -vector.y }


unit : FloatVector -> IntVector
unit vector =
    let
        x =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                1

            else
                -1

        y =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                1

            else
                -1
    in
    { x = x, y = y }


unitIntVector : IntVector -> IntVector
unitIntVector vector =
    let
        x =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                1

            else
                -1

        y =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                1

            else
                -1
    in
    { x = x, y = y }


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


getPieceFromCoordinate : Board -> Coordinate -> Maybe Piece
getPieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.coordinatePieces
                |> Dict.get (toTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.pieces) pieceIndex


getTentativePieceFromCoordinate : Board -> Coordinate -> Maybe Piece
getTentativePieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.tentativeCoordinatePieces
                |> Dict.get (toTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.tentativePieces) pieceIndex


combineVectors : FloatVector -> FloatVector -> FloatVector
combineVectors vector1 vector2 =
    { x = vector1.x + vector2.x, y = vector1.y + vector2.y }


increaseFloatVectorMagnitude : FloatVector -> FloatVector
increaseFloatVectorMagnitude vector =
    let
        newX =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                vector.x + 1

            else
                vector.x - 1

        newY =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                vector.y + 1

            else
                vector.y - 1
    in
    { x = newX, y = newY }


decreaseIntVectorMagnitude : IntVector -> IntVector
decreaseIntVectorMagnitude vector =
    let
        newX =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                vector.x - 1

            else
                vector.x + 1

        newY =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                vector.y - 1

            else
                vector.y + 1
    in
    { x = newX, y = newY }



{-------------------------------------------------
   Magnetic Field Calculation Functions
-------------------------------------------------}


calculateMagneticField : Int -> FloatVector -> FloatVector
calculateMagneticField magnetism distanceVector =
    let
        distance =
            Basics.sqrt (distanceVector.x ^ 2 + distanceVector.y ^ 2)

        magnitude =
            toFloat magnetism / (distance ^ 2)

        unitVector =
            unit distanceVector
    in
    { x = toFloat unitVector.x * magnitude, y = toFloat unitVector.y * magnitude }


pieceMagneticField : Int -> FloatVector -> Piece -> Coordinate -> Int -> List ( Coordinate, MagneticField )
pieceMagneticField magnetism distanceVector piece currentCoordinate remainingMagnetism =
    let
        vector =
            calculateMagneticField magnetism distanceVector
    in
    case remainingMagnetism of
        0 ->
            []

        _ ->
            case piece.polarity of
                Positive ->
                    ( movePieceCoordinate currentCoordinate (unit vector), { positiveVector = vector, negativeVector = { x = 0, y = 0 }, players = [ piece.player ] } ) :: pieceMagneticField magnetism (increaseFloatVectorMagnitude distanceVector) piece (movePieceCoordinate currentCoordinate (unit vector)) (remainingMagnetism - 1)

                Negative ->
                    ( movePieceCoordinate currentCoordinate (unit vector), { positiveVector = { x = 0, y = 0 }, negativeVector = vector, players = [ piece.player ] } ) :: pieceMagneticField magnetism (increaseFloatVectorMagnitude distanceVector) piece (movePieceCoordinate currentCoordinate (unit vector)) (remainingMagnetism - 1)

                None ->
                    []


pieceMagneticFieldDirs : Float -> List FloatVector
pieceMagneticFieldDirs magnitude =
    [ { x = 1, y = 0 }
    , { x = 0, y = 1 }
    , { x = -1, y = 0 }
    , { x = 0, y = -1 }
    , { x = 1, y = 1 }
    , { x = -1, y = 1 }
    , { x = -1, y = -1 }
    , { x = 1, y = -1 }
    ]


getMagneticFieldFromPiece : Board -> Int -> Coordinate -> List ( Coordinate, MagneticField )
getMagneticFieldFromPiece board magnetism pieceCoordinate =
    let
        piece =
            getPieceFromCoordinate board pieceCoordinate

        finalPiece =
            case piece of
                Just p ->
                    Just p

                Nothing ->
                    getTentativePieceFromCoordinate board pieceCoordinate
    in
    case finalPiece of
        Just p ->
            List.concatMap (\vector -> pieceMagneticField magnetism vector p pieceCoordinate magnetism) (pieceMagneticFieldDirs (toFloat magnetism))

        Nothing ->
            []


mergeMagneticFields : MagneticField -> MagneticField -> MagneticField
mergeMagneticFields field1 field2 =
    { positiveVector = { x = field1.positiveVector.x + field2.positiveVector.x, y = field1.positiveVector.y + field2.positiveVector.y }
    , negativeVector = { x = field1.negativeVector.x + field2.negativeVector.x, y = field1.negativeVector.y + field2.negativeVector.y }
    , players = field1.players ++ field2.players
    }


updateMagneticFieldDict : List ( Coordinate, MagneticField ) -> Dict ( Int, Int ) MagneticField -> Dict ( Int, Int ) MagneticField
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


getBoardMagneticFieldRec : Board -> Int -> List Coordinate -> Dict ( Int, Int ) MagneticField -> Dict ( Int, Int ) MagneticField
getBoardMagneticFieldRec board magnetism pieceCoordinates magneticFields =
    case pieceCoordinates of
        [] ->
            magneticFields

        coordinate :: rest ->
            let
                pieceFields =
                    getMagneticFieldFromPiece board magnetism coordinate
            in
            getBoardMagneticFieldRec board magnetism rest (updateMagneticFieldDict pieceFields magneticFields)


getBoardMagneticField : Board -> Int -> Dict ( Int, Int ) MagneticField
getBoardMagneticField board magnetism =
    getBoardMagneticFieldRec board magnetism (Dict.values board.pieceCoordinates ++ Dict.values board.tentativePieceCoordinates) Dict.empty


updateBoardMagneticField : Int -> Board -> Board
updateBoardMagneticField magnetism board =
    { board
        | magneticField = getBoardMagneticField board magnetism
    }


updatePiecePositions : Board -> Board
updatePiecePositions board =
    let
        coordinatePieces =
            Dict.values (Dict.map (\coordinate index -> ( coordinate, index, Dict.get index board.pieces )) board.coordinatePieces)
    in
    updatePiecePositionsRecursive coordinatePieces board.magneticField board


updatePiecePositionsRecursive : List ( ( Int, Int ), Int, Maybe Piece ) -> Dict ( Int, Int ) MagneticField -> Board -> Board
updatePiecePositionsRecursive coordinatePieces magneticFields board =
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
                                    unit (getMovementVectorForMagnet f p)
                            in
                            updatePiecePositionsRecursive rest magneticFields (movePiece board pieceIndex movementVector)

                        Nothing ->
                            updatePiecePositionsRecursive rest magneticFields board

                Nothing ->
                    updatePiecePositionsRecursive rest magneticFields board


getMovementVectorForMagnet : MagneticField -> Piece -> FloatVector
getMovementVectorForMagnet field piece =
    case piece.polarity of
        Positive ->
            combineVectors field.positiveVector (negative field.negativeVector)

        Negative ->
            combineVectors field.negativeVector (negative field.positiveVector)

        None ->
            { x = 0, y = 0 }



{-------------------------------------------------
   Piece Movement and Insertion Functions
-------------------------------------------------}


insertPiece : Piece -> Coordinate -> Board -> Board
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


insertTentativePiece : Piece -> Coordinate -> Board -> Board
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


movePieceCoordinate : Coordinate -> IntVector -> Coordinate
movePieceCoordinate coordinate vector =
    { x = coordinate.x + vector.x, y = coordinate.y + vector.y }


getMaxMovementCoordinate : Board -> Coordinate -> IntVector -> Coordinate
getMaxMovementCoordinate board coordinate vector =
    let
        tentativeNext =
            movePieceCoordinate coordinate (unitIntVector vector)

        tentativeNextPiece =
            getPieceFromCoordinate board tentativeNext
    in
    case tentativeNextPiece of
        Just _ ->
            coordinate

        Nothing ->
            coordinate



--getMaxMovementCoordinate board tentativeNext (decreaseIntVectorMagnitude vector)


movePiece : Board -> Int -> IntVector -> Board
movePiece board pieceIndex vector =
    let
        pieceCoordinate =
            Dict.get pieceIndex board.pieceCoordinates

        newPieceCoordinate =
            Maybe.map (movePieceCoordinate vector) pieceCoordinate

        newPieceCoordinateDir =
            Maybe.map (movePieceCoordinate (unitIntVector vector)) pieceCoordinate

        newPieceCoordinateExisting =
            Maybe.andThen (\coordinate -> Dict.get coordinate board.coordinatePieces) (Maybe.map toTuple newPieceCoordinate)

        maxMoveCoordinate =
            Maybe.map (\coordinate -> getMaxMovementCoordinate board coordinate vector) pieceCoordinate
    in
    case newPieceCoordinateExisting of
        Just _ ->
            board

        Nothing ->
            case pieceCoordinate of
                Just coordinate ->
                    let
                        newCoordinate =
                            movePieceCoordinate vector coordinate

                        newCoordinateOnMap =
                            newCoordinate.x >= 0 && newCoordinate.x < board.config.gridDimensions && newCoordinate.y >= 0 && newCoordinate.y < board.config.gridDimensions
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
        numPlayers =
            toFloat (List.length field.players)
    in
    field.players
        |> List.map (\player -> ( player, 1 / numPlayers ))
        |> Dict.fromList


getBoardScores : Board -> Dict Int Float
getBoardScores board =
    let
        playerDict =
            Dict.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]
    in
    Dict.foldl (\coordinate field existing -> addDicts existing (getFieldScore field)) playerDict board.magneticField



{-------------------------------------------------
   View Helper Functions
-------------------------------------------------}


determineCellContent : Dict ( Int, Int ) MagneticField -> Board -> Coordinate -> CellContent
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



{-
   Cell Grid Specific View Functions
-}


getCellGrid : Board -> CellGrid CellContent
getCellGrid board =
    CellGrid.initialize (Dimensions board.config.gridDimensions board.config.gridDimensions) (\i j -> determineCellContent board.magneticField board (fromArgs j i))


boardHtml : Board -> Html Msg
boardHtml board =
    Html.map CellGridMessage (MyCellGrid.asHtml { width = board.config.displaySize, height = board.config.displaySize } (cellStyle board) (getCellGrid board))


getCellColorFromContent : CellContent -> Color.Color
getCellColorFromContent content =
    case content of
        GridPiece piece ->
            Color.white

        NoContent ->
            Color.white

        GridMagneticField field ->
            List.map (\player -> getPlayerColor player) field.players
                |> getMergedCellColor

        PieceOnField _ field ->
            List.map (\player -> getPlayerColor player) field.players
                |> getMergedCellColor


getCellOpacityFromContent : Float -> CellContent -> Float
getCellOpacityFromContent opacity content =
    case content of
        GridPiece piece ->
            opacity

        NoContent ->
            opacity

        GridMagneticField field ->
            1 - (opacity ^ toFloat (List.length field.players))

        PieceOnField _ field ->
            1 - (opacity ^ toFloat (List.length field.players))


getPieceColorFromContent : CellContent -> Color.Color
getPieceColorFromContent content =
    case content of
        GridPiece piece ->
            playerColorToColor (getPlayerColor piece.player)

        PieceOnField piece _ ->
            playerColorToColor (getPlayerColor piece.player)

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


cellStyle : Board -> MyCellGrid.CellStyle CellContent
cellStyle board =
    { toCellColor = \z -> getCellColorFromContent z
    , toPieceColor = \z -> getPieceColorFromContent z
    , toCellOpacity = \z -> getCellOpacityFromContent 0.7 z
    , toText = \content -> getTextFromContent content
    , cellWidth = toFloat (board.config.displaySize // board.config.gridDimensions)
    , cellHeight = toFloat (board.config.displaySize // board.config.gridDimensions)
    , gridLineColor = Color.rgb 0 0 0
    , gridLineWidth = 5
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
