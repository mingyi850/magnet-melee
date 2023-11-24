module Game exposing (..)

{-| This file handles all the game logic and provides the Gameplay interface to the Main application.alias.

The core parts you need to implement are:

1.  A type for your Game model
2.  An initialisation function that takes a Settings record and returns a Game record
3.  A Msg type that represents all the possible messages that can be sent from the interface to the game logic
4.  An update function that takes a Msg and a Game and returns a new Game
5.  A view function that takes a Game and returns Html Msg (the interface for the game)

You'll probably want to implement a lot of helper functions to make the above easier.

-}

import CellGrid exposing (..)
import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Models exposing (..)
import MyCellGrid exposing (Msg)
import Settings exposing (..)



--------------------------------------------------------------------------------
-- GAME MODEL
--------------------------------------------------------------------------------


{-| A record type which contains all of the game state.

This needs to be sufficiently detailed to represent the entire game state, i.e.
if you save this record, turn off your computer, and then reload this record,
you should be able to pick up the game exactly where you left off.

We also need some metadata including the settings used to initialise
the game, the status (whether it's still going or completed), and
whose turn it currently is.

You might also like to pre-calculate some data and store it here
if you will use it a lot.

-}
type alias Game =
    { settings : Settings
    , gridSize : Int
    , magnetism : Int
    , board : Board
    , totalMoves : Int
    , maxMoves : Int
    , turn : Int
    , players : Int
    , playerPolarity : Polarity
    }


type alias GameMoveResult =
    { status : Status
    , game : Game
    }


{-| Create the initial game data given the settings.
-}
init : Settings -> ( Game, Cmd Msg )
init settings =
    let
        initialGame =
            { settings = settings
            , gridSize = settings.gridSize
            , magnetism = settings.magnetism
            , board = emptyBoard (getBoardConfig settings)
            , totalMoves = 0
            , maxMoves = settings.maxMoves
            , turn = 0
            , players = settings.players
            , playerPolarity = Negative
            }
    in
    ( initialGame, Cmd.none )



--------------------------------------------------------------------------------
-- GAME LOGIC
--------------------------------------------------------------------------------


{-| The possible moves that a player can make.
-}
type Move
    = Increment
    | Decrement
    | Multiply
    | SelectPolarity Polarity
    | PlacePiece Coordinate


type Status
    = Success
    | Failure


{-| Apply a move to a game state, returning a new game state.
-}
applyMove : Move -> Game -> GameMoveResult
applyMove move game =
    case move of
        Increment ->
            { status = Success, game = { game | gridSize = game.gridSize + 1, magnetism = game.magnetism + 1 } }

        Decrement ->
            { status = Success, game = { game | gridSize = game.gridSize - 1, magnetism = game.magnetism + 1 } }

        Multiply ->
            { status = Success, game = { game | gridSize = game.gridSize * 2, magnetism = game.magnetism + 1 } }

        SelectPolarity polarity ->
            { status = Success, game = { game | playerPolarity = polarity } }

        PlacePiece coordinate ->
            case getPieceFromCoordinate game.board coordinate of
                Just piece ->
                    { status = Failure, game = game }

                Nothing ->
                    { status = Success, game = { game | board = insertPiece { color = determinePlayerColor game, polarity = game.playerPolarity } coordinate game.board } }


determinePlayerColor : Game -> PlayerColor
determinePlayerColor game =
    case game.turn of
        0 ->
            Red

        1 ->
            Blue

        2 ->
            Green

        3 ->
            Yellow

        _ ->
            White



--------------------------------------------------------------------------------
-- INTERFACE LOGIC
--
-- This section deals with how to map the interface to the game logic.
--
-- Msg contains messages that can be sent from the game interface. You should then
-- choose how to handle them in terms of game logic.
--
-- This also sets scaffolding for the computer players - when a computer player
-- makes a move, they generate a message (ReceivedComputerMove) which is then handled
-- just like a player interacting with the interface.
--------------------------------------------------------------------------------


{-| An enumeration of all messages that can be sent from the interface to the game
-}
type Msg
    = ClickedIncrement
    | ClickedDecrement
    | ClickedMultiply
    | ModelMsg Models.Msg
    | SelectedPolarity Polarity


{-| A convenience function to pipe a command into a (Game, Cmd Msg) tuple.
-}
withCmd : Cmd Msg -> GameMoveResult -> ( Game, Cmd Msg )
withCmd cmd { status, game } =
    ( game, cmd )



{- Get Cell Coordinates from CellGrid Click -}


determineCellCoordinates : MyCellGrid.Msg -> Coordinate
determineCellCoordinates cellMsg =
    { x = cellMsg.cell.column
    , y = cellMsg.cell.row
    }


updateSuccessfulMove : GameMoveResult -> GameMoveResult
updateSuccessfulMove { status, game } =
    case status of
        Success ->
            { status = status
            , game =
                { game
                    | totalMoves = game.totalMoves + 1
                    , turn = modBy game.players (game.totalMoves + 1)
                }
            }

        Failure ->
            { status = status, game = game }


{-| The main update function for the game, which takes an interface message and returns
a new game state as well as any additional commands to be run.
-}
update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        ClickedIncrement ->
            game
                |> applyMove Increment
                |> withCmd Cmd.none

        ClickedDecrement ->
            game
                |> applyMove Decrement
                |> withCmd Cmd.none

        ClickedMultiply ->
            withCmd Cmd.none (applyMove Multiply game)

        SelectedPolarity polarity ->
            game
                |> applyMove (SelectPolarity polarity)
                |> withCmd Cmd.none

        ModelMsg modelMsg ->
            case modelMsg of
                CellGridMessage cellmsg ->
                    game
                        |> applyMove (PlacePiece (determineCellCoordinates cellmsg))
                        |> updateSuccessfulMove
                        |> withCmd Cmd.none



--------------------------------------------------------------------------------
-- GAME VIEW FUNCTION
--------------------------------------------------------------------------------


{-| The main view function that gets called from the Main application.

Essentially, takes a game and projects it into a HTML interface where Messages
can be sent from.

-}
getBoardConfig : Settings -> BoardConfig
getBoardConfig settings =
    { displaySize = Basics.max 800 (settings.gridSize * 20)
    , gridDimensions = settings.gridSize
    }


getBoardView : Game -> Html Msg
getBoardView game =
    Html.map ModelMsg
        (div [ id "board-container" ]
            [ boardHtml game.board
            ]
        )


type alias GameDropdownGenericConfig enum =
    { label : String
    , onSelect : enum -> Msg
    , toString : enum -> String
    , fromString : String -> enum
    , current : enum
    , options : List ( String, enum )
    }


polarityDropDownConfig : Game -> GameDropdownGenericConfig Polarity
polarityDropDownConfig game =
    { label = "Player Polarity"
    , onSelect = \polarity -> SelectedPolarity polarity
    , toString = \polarity -> toPolarityString polarity
    , fromString = \polarityString -> fromPolarityString polarityString
    , current = game.playerPolarity
    , options = [ ( "Positive", Positive ), ( "Negative", Negative ), ( "None", None ) ]
    }


type alias GamePickChoiceDropdownConfig =
    { label : String
    , onSelect : String -> Msg
    , options : List PickChoiceDropdownOption
    }


genericConfigToDropdownConfig : GameDropdownGenericConfig enum -> GamePickChoiceDropdownConfig
genericConfigToDropdownConfig { label, onSelect, toString, fromString, current, options } =
    { label = label
    , onSelect = fromString >> onSelect
    , options = List.map (\( optionLabel, value ) -> { label = optionLabel, value = toString value, isSelected = value == current }) options
    }


viewPolarityDropdown : GamePickChoiceDropdownConfig -> Html Msg
viewPolarityDropdown config =
    div [ class "setting-picker-item" ]
        [ label [ class "setting-picker-item-label" ] [ text config.label ]
        , select [ class "setting-picker-item-input setting-picker-item-input-select", onInput config.onSelect ]
            (List.map
                (\optionData ->
                    option [ value optionData.value, selected optionData.isSelected ] [ text optionData.label ]
                )
                config.options
            )
        ]


getPolarityDropdown : Game -> Html Msg
getPolarityDropdown game =
    let
        config =
            genericConfigToDropdownConfig (polarityDropDownConfig game)
    in
    viewPolarityDropdown config


view : Game -> Html Msg
view game =
    div [ id "game-screen-container" ]
        [ h1 [ id "counter-value" ] [ text ("Game Remaining Moves " ++ String.fromInt (game.maxMoves - game.totalMoves)) ]
        , h2 [ id "total-moves-value" ] [ text ("Total Moves: " ++ String.fromInt game.totalMoves) ]
        , div [ id "counter-buttons" ]
            [ button [ onClick ClickedDecrement ] [ text "-" ]
            , button [ onClick ClickedIncrement ] [ text "+" ]
            , button [ onClick ClickedMultiply ] [ text "x2" ]
            ]
        , div [ id "polarity-dropdown" ]
            [ getPolarityDropdown game ]
        , getBoardView game
        ]
