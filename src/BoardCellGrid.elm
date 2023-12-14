module BoardCellGrid exposing (..)

{-| Render a cell grid as html using SVG

SVG is slower for large cell grids, but is more interactive. User clicks on cells in the grid can be captured and used for interaction.

@docs asHtml, asSvg
@docs Msg, CellStyle

-}

import CellGrid exposing (CellGrid(..), Position)
import Color exposing (Color)
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Svg exposing (Svg)
import Svg.Attributes


{-| Customize how a cell is rendered.
`Color` is as defined in the package `avh4/elm-color`, e.g. `Color.rgb 1 0 0` is bright red.

    cellStyle : CellStyle Bool
    cellStyle =
        { toColor =
            \b ->
                if b then
                    Color.green

                else
                    Color.red
        , cellWidth = 10
        , cellHeight = 10
        , gridLineWidth = 1
        , gridLineColor = Color.black
        }

-}
type alias CellStyle a =
    { cellWidth : Float
    , cellHeight : Float
    , toCellColor : a -> Color
    , toPieceColor : a -> Color
    , toPieceTextColor : a -> Color
    , toPieceScale : a -> Float
    , toCellOpacity : a -> Float
    , toText : a -> String
    , gridLineWidth : Float
    , pieceLineWidth : Float
    , gridLineColor : Color
    , shouldRenderPiece : a -> Bool
    }


type InteractionType
    = Click
    | Hover


{-| Capture clicks on the rendered cell grid. Gives the position in the cell grid, and the local `(x, y)` coordinates of the cell
-}
type alias Msg =
    { cell : Position
    , coordinates :
        { x : Float
        , y : Float
        }
    , interaction : InteractionType
    }


{-| Render a cell grid into an html element of the given width and height.
-}
asHtml : { width : Int, height : Int } -> CellStyle a -> CellGrid a -> Html Msg
asHtml { width, height } cr cellGrid =
    Svg.svg
        [ Svg.Attributes.height (String.fromInt height)
        , Svg.Attributes.width (String.fromInt width)
        , Svg.Attributes.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
        ]
        [ asSvg cr cellGrid ]


{-| Render a cell grid as an svg `<g>` element, useful for integration with other svg.
-}
asSvg : CellStyle a -> CellGrid a -> Svg Msg
asSvg style cellGrid =
    let
        boxes =
            CellGrid.indexedMap (\i j -> renderCell style (Position i j)) cellGrid
                |> CellGrid.foldr (::) []

        pieces =
            CellGrid.indexedMap (\i j -> renderPiece style (Position i j)) cellGrid
                |> CellGrid.foldr (++) []
    in
    Svg.g [] (boxes ++ pieces)


onMouseDown : Position -> Mouse.Event -> Msg
onMouseDown position =
    \r ->
        let
            ( x, y ) =
                r.clientPos
        in
        { cell = position, coordinates = { x = x, y = y }, interaction = Click }


onMouseHover : Position -> Mouse.Event -> Msg
onMouseHover position =
    \r ->
        let
            ( x, y ) =
                r.clientPos
        in
        { cell = position, coordinates = { x = x, y = y }, interaction = Hover }


renderPiece : CellStyle a -> Position -> a -> List (Svg Msg)
renderPiece style position value =
    if style.shouldRenderPiece value then
        [ Svg.circle
            [ Svg.Attributes.cx (String.fromFloat (style.cellWidth * toFloat position.column + (style.cellWidth / 2)))
            , Svg.Attributes.cy (String.fromFloat (style.cellHeight * toFloat position.row + (style.cellHeight / 2)))
            , Svg.Attributes.r (String.fromFloat (style.cellWidth * style.toPieceScale value))
            , Svg.Attributes.strokeWidth (String.fromFloat style.pieceLineWidth)
            , Svg.Attributes.fill (toCssString (style.toPieceColor value))
            , Svg.Attributes.stroke (toCssString (style.toPieceTextColor value))
            , Svg.Attributes.fillOpacity "1"
            , Svg.Attributes.z "500"
            , Mouse.onDown (onMouseDown position)
            , Mouse.onOver (onMouseHover position)
            ]
            []
        , Svg.text_
            [ Svg.Attributes.x (String.fromFloat (style.cellWidth * toFloat position.column + (style.cellWidth / 2)))
            , Svg.Attributes.y (String.fromFloat (style.cellHeight * toFloat position.row + (style.cellHeight / 2)))
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.alignmentBaseline "middle"
            , Svg.Attributes.fontSize (String.fromFloat (style.cellWidth * 2.5 * style.toPieceScale value))
            , Svg.Attributes.fontWeight "bold"
            , Svg.Attributes.fontFamily "Consolas, monospace"
            , Svg.Attributes.strokeWidth (String.fromFloat (style.gridLineWidth * 2.0))
            , Svg.Attributes.fill (toCssString (style.toPieceTextColor value))
            , Svg.Attributes.z "1000"
            , Mouse.onDown (onMouseDown position)
            , Mouse.onOver (onMouseHover position)
            ]
            [ Svg.text (style.toText value) ]
        ]

    else
        []


renderCell : CellStyle a -> Position -> a -> Svg Msg
renderCell style position value =
    Svg.rect
        [ Svg.Attributes.width (String.fromFloat style.cellWidth)
        , Svg.Attributes.height (String.fromFloat style.cellHeight)
        , Svg.Attributes.x (String.fromFloat (style.cellWidth * toFloat position.column))
        , Svg.Attributes.y (String.fromFloat (style.cellHeight * toFloat position.row))
        , Svg.Attributes.strokeWidth (String.fromFloat style.gridLineWidth)
        , Svg.Attributes.fill (toCssString (style.toCellColor value))
        , Svg.Attributes.stroke (toCssString style.gridLineColor)
        , Svg.Attributes.fillOpacity "1" --(style.toCellOpacity value |> String.fromFloat)
        , Mouse.onDown (onMouseDown position)
        , Mouse.onOver (onMouseHover position)
        ]
        []


{-| Use a faster toCssString

Using `++` instead of `String.concat` which avh4/color uses makes this much faster.

-}
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
