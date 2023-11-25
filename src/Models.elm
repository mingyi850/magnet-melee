module Models exposing (..)

import CellGrid exposing (..)
import Color exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import MyCellGrid exposing (..)


type Msg
    = CellGridMessage MyCellGrid.Msg


type PlayerColor
    = Red
    | Blue
    | Green
    | Yellow
    | White


type alias MagneticField =
    { positiveVector : Vector
    , negativeVector : Vector
    , players : List PlayerColor
    }


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


type alias Piece =
    { color : PlayerColor
    , polarity : Polarity
    }


type alias Board =
    { pieces : Dict Int Piece
    , pieceCoordinates : Dict Int Coordinate
    , coordinatePieces : Dict ( Int, Int ) Int
    , config : BoardConfig
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


type alias Vector =
    { x : Int
    , y : Int
    }


emptyBoard : BoardConfig -> Board
emptyBoard boardConfig =
    { pieces = Dict.empty
    , pieceCoordinates = Dict.empty
    , coordinatePieces = Dict.empty
    , config = boardConfig
    }


getPieceFromCoordinate : Board -> Coordinate -> Maybe Piece
getPieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.coordinatePieces
                |> Dict.get (toTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.pieces) pieceIndex


pieceMagneticField : Vector -> Piece -> Coordinate -> Int -> List ( Coordinate, MagneticField )
pieceMagneticField vector piece currentCoordinate remainingMagnetism =
    case remainingMagnetism of
        0 ->
            []

        _ ->
            case piece.polarity of
                Positive ->
                    ( movePieceCoordinate currentCoordinate vector, { positiveVector = vector, negativeVector = { x = 0, y = 0 }, players = [ piece.color ] } ) :: pieceMagneticField vector piece (movePieceCoordinate currentCoordinate vector) (remainingMagnetism - 1)

                Negative ->
                    ( movePieceCoordinate currentCoordinate vector, { positiveVector = { x = 0, y = 0 }, negativeVector = vector, players = [ piece.color ] } ) :: pieceMagneticField vector piece (movePieceCoordinate currentCoordinate vector) (remainingMagnetism - 1)

                None ->
                    []


pieceMagneticFieldDirs : List Vector
pieceMagneticFieldDirs =
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
    in
    case piece of
        Just p ->
            List.concatMap (\vector -> pieceMagneticField vector p pieceCoordinate magnetism) pieceMagneticFieldDirs

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
    getBoardMagneticFieldRec board magnetism (Dict.values board.pieceCoordinates) Dict.empty


determineCellContent : Dict ( Int, Int ) MagneticField -> Board -> Coordinate -> CellContent
determineCellContent magneticField board coordinate =
    let
        piece =
            getPieceFromCoordinate board coordinate

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
            case field of
                Just f ->
                    GridMagneticField f

                Nothing ->
                    NoContent


getCellGrid : Int -> Board -> CellGrid CellContent
getCellGrid magnetism board =
    let
        magneticField =
            getBoardMagneticField board magnetism
    in
    CellGrid.initialize (Dimensions board.config.gridDimensions board.config.gridDimensions) (\i j -> determineCellContent magneticField board (fromArgs j i))


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


movePieceCoordinate : Coordinate -> Vector -> Coordinate
movePieceCoordinate coordinate vector =
    { x = coordinate.x + vector.x, y = coordinate.y + vector.y }


movePiece : Board -> Int -> Vector -> Board
movePiece board pieceIndex vector =
    let
        pieceCoordinate =
            Dict.get pieceIndex board.pieceCoordinates
    in
    { board
        | pieceCoordinates = Dict.update pieceIndex (Maybe.map (movePieceCoordinate vector)) board.pieceCoordinates
        , coordinatePieces =
            case pieceCoordinate of
                Just coordinate ->
                    let
                        newCoordinate =
                            movePieceCoordinate vector coordinate
                    in
                    Dict.insert (toTuple newCoordinate) pieceIndex (Dict.remove (toTuple coordinate) board.coordinatePieces)

                Nothing ->
                    board.coordinatePieces
    }



-- View Helpers


boardHtml : Int -> Board -> Html Msg
boardHtml magnetism board =
    Html.map CellGridMessage (MyCellGrid.asHtml { width = board.config.displaySize, height = board.config.displaySize } (cellStyle board) (getCellGrid magnetism board))


getCellColorFromContent : CellContent -> Color.Color
getCellColorFromContent content =
    case content of
        GridPiece piece ->
            playerColorToColor piece.color

        NoContent ->
            Color.white

        GridMagneticField field ->
            field.players
                |> getMergedCellColor

        PieceOnField _ field ->
            field.players
                |> getMergedCellColor


getPieceColorFromContent : CellContent -> Color.Color
getPieceColorFromContent content =
    case content of
        GridPiece piece ->
            playerColorToColor piece.color

        PieceOnField piece _ ->
            playerColorToColor piece.color

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
    , toText = \content -> getTextFromContent content
    , cellWidth = toFloat (board.config.displaySize // board.config.gridDimensions)
    , cellHeight = toFloat (board.config.displaySize // board.config.gridDimensions)
    , gridLineColor = Color.rgb 0 0 0
    , gridLineWidth = 0.5
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
