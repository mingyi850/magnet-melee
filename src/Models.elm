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


playerColorToColor : PlayerColor -> Color.Color
playerColorToColor color =
    case color of
        Red ->
            Color.rgb 255 0 0

        Blue ->
            Color.rgb 0 0 255

        Green ->
            Color.rgb 0 255 0

        Yellow ->
            Color.rgb 255 255 0

        White ->
            Color.rgb 255 255 255


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



{-
   initializeCoordinates: Int -> Dict (Int, Int) Int
   initializeCoordinates size =
       let
           coordinates =
               List.range 0 size
                   |> List.map (\x -> List.range 0 size |> List.map (\y -> { x = x, y = y }))
                   |> List.concat
       in
           coordinates
               |> List.map (\coordinate -> (coordinate, Nothing))
               |> List.map (\(coordinate, value) -> (toTuple { x = coordinate.x, y = coordinate.y }, value))
               |> Dict.fromList
-}


getPieceFromCoordinate : Board -> Coordinate -> Maybe Piece
getPieceFromCoordinate board coordinate =
    let
        pieceIndex =
            board.coordinatePieces
                |> Dict.get (toTuple coordinate)
    in
    Maybe.andThen (\index -> Dict.get index board.pieces) pieceIndex


getCellGrid : Board -> CellGrid (Maybe Piece)
getCellGrid board =
    CellGrid.initialize (Dimensions board.config.gridDimensions board.config.gridDimensions) (\i j -> getPieceFromCoordinate board (fromArgs j i))


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
movePieceCoordinate vector coordinate =
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


boardHtml : Board -> Html Msg
boardHtml board =
    Html.map CellGridMessage (MyCellGrid.asHtml { width = board.config.displaySize, height = board.config.displaySize } (cellStyle board) (getCellGrid board))


getColorFromMaybePiece : Maybe Piece -> Color.Color
getColorFromMaybePiece maybePiece =
    case maybePiece of
        Just piece ->
            playerColorToColor piece.color

        Nothing ->
            Color.rgb 1 1 1


cellStyle : Board -> MyCellGrid.CellStyle (Maybe Piece)
cellStyle board =
    { toColor = \z -> getColorFromMaybePiece z
    , toText = \piece -> Maybe.withDefault "" (Maybe.map (\p -> toPolarityIcon p.polarity) piece)
    , cellWidth = toFloat (board.config.displaySize // board.config.gridDimensions)
    , cellHeight = toFloat (board.config.displaySize // board.config.gridDimensions)
    , gridLineColor = Color.rgb 0 0 0
    , gridLineWidth = 0.5
    }
