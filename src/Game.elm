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

import Array exposing (..)
import CellGrid exposing (..)
import Common exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Models exposing (..)
import MyCellGrid exposing (InteractionType(..), Msg)
import Process
import Random exposing (..)
import Round exposing (..)
import Settings exposing (..)
import Svg exposing (..)
import Svg.Attributes
import Task
import Utils exposing (..)



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
    , turn : Int
    , players : Dict Int Player
    , playerPolarity : Polarity
    , status : GameStatus
    , time : Int
    , randomMove : RandomMove
    }


type alias GameMoveResult =
    { status : Status
    , game : Game
    }


type alias RandomMove =
    { move : MoveData
    , seed : Seed
    }


type Agent
    = Human
    | AIEasy
    | AIMedium


getAgentFromInt : Int -> Agent
getAgentFromInt agent =
    case agent of
        0 ->
            Human

        1 ->
            AIEasy

        2 ->
            AIMedium

        _ ->
            Human


type alias Player =
    { remainingMoves : Int
    , polarity : Polarity
    , score : Float
    , agent : Agent
    }


type GameStatus
    = HumanMove
    | Processing
    | GameOver
    | AI Int


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
            , turn = 0
            , players = range 0 (settings.players - 1) |> List.map (\player -> ( player, { remainingMoves = settings.maxMoves, polarity = Negative, score = 0.0, agent = getAgentFromInt (Maybe.withDefault 0 (Array.get player settings.playerAI)) } )) |> Dict.fromList
            , playerPolarity = Negative
            , status = HumanMove
            , time = 0
            , randomMove = { move = { x = settings.gridSize // 2, y = settings.gridSize // 2, polarity = Negative }, seed = initialSeed settings.time }
            }
    in
    ( initialGame, Cmd.none )



--------------------------------------------------------------------------------
-- GAME LOGIC
--------------------------------------------------------------------------------


{-| The possible moves that a player can make.
-}
type Move
    = SelectPolarity Int Polarity
    | PlacePiece Coordinate Polarity
    | DisplayPiece Coordinate Polarity


type Status
    = Success
    | Failure


type alias MoveData =
    { x : Int
    , y : Int
    , polarity : Polarity
    }


{-| Apply a move to a game state, returning a new game state.
-}
applyMove : Move -> Game -> GameMoveResult
applyMove move game =
    case move of
        SelectPolarity player polarity ->
            { status = Success, game = updatePlayerPolarity player polarity game }

        PlacePiece coordinate polarity ->
            case getPieceFromCoordinate game.board coordinate of
                Just piece ->
                    { status = Failure, game = game }

                Nothing ->
                    { status = Success
                    , game =
                        { game
                            | board =
                                removeTentativePieces game.board
                                    |> insertPiece { player = game.turn, polarity = polarity } coordinate
                        }
                    }

        DisplayPiece coordinate polarity ->
            case getPieceFromCoordinate game.board coordinate of
                Just piece ->
                    { status = Failure, game = game }

                Nothing ->
                    { status = Success, game = { game | board = insertTentativePiece { player = game.turn, polarity = polarity } coordinate game.board |> updateBoardMagneticField game.magnetism } }


generateRandomMove : Game -> RandomMove
generateRandomMove game =
    let
        randomCoordinate =
            Random.step (Random.pair (Random.int 0 game.board.config.gridDimensions) (Random.int 0 game.board.config.gridDimensions)) game.randomMove.seed
                |> (\( ( x, y ), seed ) -> { x = x, y = y, seed = seed })

        randomPolarity =
            if Tuple.first (Random.step (Random.int 0 1) randomCoordinate.seed) == 0 then
                Negative

            else
                Positive
    in
    { move = { x = randomCoordinate.x, y = randomCoordinate.y, polarity = randomPolarity }, seed = randomCoordinate.seed }


generateRandomCoordinate : Game -> Cmd Coordinate
generateRandomCoordinate game =
    Random.generate (\( x, y ) -> { x = x, y = y })
        (Random.pair (Random.int 0 game.board.config.gridDimensions) (Random.int 0 game.board.config.gridDimensions))


getGameScore : Game -> Dict Int Float
getGameScore game =
    let
        scores =
            Dict.fromList (range 0 (Dict.size game.players - 1) |> List.map (\player -> ( player, 0.0 )))

        boardScores =
            getBoardScores game.board
    in
    addDicts scores boardScores


getPlayerPolarity : Int -> Game -> Polarity
getPlayerPolarity player game =
    Maybe.withDefault Negative (Maybe.map (\playerData -> playerData.polarity) (Dict.get player game.players))


getPlayerAgency : Int -> Game -> Agent
getPlayerAgency player game =
    Maybe.withDefault Human (Maybe.map (\playerData -> playerData.agent) (Dict.get player game.players))



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
    = ModelMsg Models.Msg
    | GetAIMove
    | PlayAIMove
    | UpdateBoard Int (List Board)
    | UpdatePlayerPolarity Int Polarity


{-| A convenience function to pipe a command into a (Game, Cmd Msg) tuple.
-}
withCmd : Cmd Msg -> Game -> ( Game, Cmd Msg )
withCmd cmd game =
    ( game, cmd )



{- Get Cell Coordinates from CellGrid Click -}


determineCellCoordinates : MyCellGrid.Msg -> Coordinate
determineCellCoordinates cellMsg =
    { x = cellMsg.cell.column
    , y = cellMsg.cell.row
    }


progressGameSuccess : Int -> Game -> ( Game, Cmd Msg )
progressGameSuccess player game =
    { game
        | players =
            Dict.update player (Maybe.map (\playerData -> { playerData | remainingMoves = playerData.remainingMoves - 1 })) game.players
        , totalMoves = game.totalMoves + 1
        , turn = modBy (Dict.size game.players) (game.totalMoves + 1)
        , status = Processing
    }
        |> checkGameOver
        |> withCmd (send 200.0 (UpdateBoard 1 []))


processMoveResult : Int -> (Int -> Game -> ( Game, Cmd Msg )) -> (Int -> Game -> ( Game, Cmd Msg )) -> GameMoveResult -> ( Game, Cmd Msg )
processMoveResult player onSuccess onFailure moveResult =
    case moveResult.status of
        Success ->
            onSuccess player moveResult.game

        Failure ->
            onFailure player moveResult.game


updateSuccessfulMove : Int -> GameMoveResult -> GameMoveResult
updateSuccessfulMove player { status, game } =
    case status of
        Success ->
            { status = status
            , game =
                { game
                    | players =
                        Dict.update player (Maybe.map (\playerData -> { playerData | remainingMoves = playerData.remainingMoves - 1 })) game.players
                    , totalMoves = game.totalMoves + 1
                    , turn = modBy (Dict.size game.players) (game.totalMoves + 1)
                    , status = Processing
                }
                    |> checkGameOver
            }

        Failure ->
            { status = status, game = game }


updatePlayerScores : Dict Int Player -> Dict Int Float -> Dict Int Player
updatePlayerScores players scoreDict =
    Dict.map
        (\player playerData ->
            { playerData
                | score = Dict.get player scoreDict |> Maybe.withDefault 0.0
            }
        )
        players


updateGameBoardMagneticField : Game -> GameMoveResult
updateGameBoardMagneticField game =
    { status = Success
    , game =
        { game | board = updateBoardMagneticField game.magnetism game.board }
    }


updateGameBoard : Int -> Game -> GameMoveResult
updateGameBoard magnitude game =
    let
        gameScores =
            getGameScore game

        newGame =
            { game
                | board = updateBoardMagneticField game.magnetism (updatePiecePositions magnitude (updateBoardMagneticField game.magnetism game.board))
                , players = updatePlayerScores game.players gameScores
            }
    in
    if newGame == game then
        case game.status of
            Processing ->
                { status = Success, game = { newGame | status = getGameStatus game } }

            _ ->
                { status = Success, game = newGame }
        -- No more changes, stop sending update message

    else
        { status = Success, game = newGame }


getGameStatus : Game -> GameStatus
getGameStatus game =
    case getPlayerAgency game.turn game of
        Human ->
            HumanMove

        AIEasy ->
            AI 0

        AIMedium ->
            AI 1


updatePlayerPolarity : Int -> Polarity -> Game -> Game
updatePlayerPolarity player polarity game =
    { game
        | players =
            Dict.update player (Maybe.map (\playerData -> { playerData | polarity = polarity })) game.players
    }


checkGameOver : Game -> Game
checkGameOver game =
    -- Check if all remaining moves for all players is 0 or less
    if Dict.foldl (\player playerData accum -> (playerData.remainingMoves <= 0) && accum) True game.players then
        { game | status = GameOver }

    else
        game


send : Float -> msg -> Cmd msg
send wait msg =
    Process.sleep wait
        |> Task.perform (\_ -> msg)


{-| The main update function for the game, which takes an interface message and returns
a new game state as well as any additional commands to be run.
-}
update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        UpdatePlayerPolarity player polarity ->
            game
                |> applyMove (SelectPolarity player polarity)
                |> alwaysCommand Cmd.none

        ModelMsg modelMsg ->
            case modelMsg of
                CellGridMessage cellmsg ->
                    case game.status of
                        HumanMove ->
                            case cellmsg.interaction of
                                Click ->
                                    game
                                        |> applyMove (PlacePiece (determineCellCoordinates cellmsg) (getPlayerPolarity game.turn game))
                                        |> processMoveResult game.turn progressGameSuccess (\_ _ -> ( game, Cmd.none ))

                                Hover ->
                                    game
                                        |> applyMove (DisplayPiece (determineCellCoordinates cellmsg) (getPlayerPolarity game.turn game))
                                        |> alwaysCommand Cmd.none

                        _ ->
                            ( game, Cmd.none )

        UpdateBoard magnitude previousStates ->
            if List.member game.board previousStates then
                updateGameBoard (magnitude + 1) game
                    |> withDetermineUpdateCommand magnitude previousStates

            else
                updateGameBoard magnitude game
                    |> withDetermineUpdateCommand magnitude previousStates

        GetAIMove ->
            case game.status of
                AI x ->
                    let
                        aiMove =
                            getAIMove x game
                    in
                    applyMove (PlacePiece { x = aiMove.x, y = aiMove.y } aiMove.polarity) game
                        |> processMoveResult game.turn progressGameSuccess (\_ _ -> ( game, Cmd.none ))

                _ ->
                    ( game, Cmd.none )

        PlayAIMove ->
            case game.status of
                AI 0 ->
                    let
                        aiMove =
                            game.randomMove.move
                    in
                    applyMove (PlacePiece { x = aiMove.x, y = aiMove.y } aiMove.polarity) game
                        |> processMoveResult game.turn progressGameSuccess sendGetAIMove

                AI 1 ->
                    let
                        aiMove =
                            getAIMove 1 game
                    in
                    applyMove (PlacePiece { x = aiMove.x, y = aiMove.y } aiMove.polarity) game
                        |> processMoveResult game.turn progressGameSuccess sendGetAIMove

                _ ->
                    ( game, Cmd.none )


sendGetAIMove : Int -> Game -> ( Game, Cmd Msg )
sendGetAIMove player game =
    ( game, send 200.0 (UpdateBoard 1 []) )


alwaysCommand : Cmd Msg -> GameMoveResult -> ( Game, Cmd Msg )
alwaysCommand msg { game, status } =
    ( game, msg )


getAIMove : Int -> Game -> MoveData
getAIMove level game =
    case level of
        0 ->
            game.randomMove.move

        1 ->
            getAIMoveEasy game.board

        _ ->
            getAIMoveEasy game.board


withDetermineUpdateCommand : Int -> List Board -> GameMoveResult -> ( Game, Cmd Msg )
withDetermineUpdateCommand magnitude previousStates gameMoveResult =
    withCmd (determineUpdateComand magnitude previousStates gameMoveResult.game) gameMoveResult.game


determineUpdateComand : Int -> List Board -> Game -> Cmd Msg
determineUpdateComand magnitude previousStates game =
    case game.status of
        Processing ->
            send 100.0 (UpdateBoard magnitude (List.append previousStates [ game.board ]))

        AI _ ->
            send 100.0 GetAIMove

        _ ->
            Cmd.none



--------------------------------------------------------------------------------
-- GAME VIEW FUNCTION
--------------------------------------------------------------------------------


{-| The main view function that gets called from the Main application.

Essentially, takes a game and projects it into a HTML interface where Messages
can be sent from.

-}
getBoardConfig : Settings -> BoardConfig
getBoardConfig settings =
    { displaySize = Basics.max 600 settings.gridSize
    , gridDimensions = settings.gridSize
    }


getBoardView : Game -> Html Msg
getBoardView game =
    Html.map ModelMsg
        (div [ id "board-container", class "game-board" ]
            [ boardHtml game.board ]
        )


type alias GamePickChoiceOptionButton =
    { label : String
    , onSelect : Msg
    , isSelected : Bool
    }


type alias GamePickChoiceButtonsConfig =
    { label : String
    , options : List GamePickChoiceOptionButton
    }


polarityPickChoiceConfig : Int -> Game -> GamePickChoiceButtonsConfig
polarityPickChoiceConfig player game =
    { label = "Player Polarity"
    , options =
        [ { label = "+", onSelect = UpdatePlayerPolarity player Positive, isSelected = getPlayerPolarity player game == Positive }
        , { label = "-", onSelect = UpdatePlayerPolarity player Negative, isSelected = getPlayerPolarity player game == Negative }
        ]
    }


viewPolaritySelector : GamePickChoiceButtonsConfig -> Html Msg
viewPolaritySelector data =
    div [ class "setting-picker-item" ]
        [ label [ id "polarity-picker-label", class "setting-picker-item-label" ] [ Html.text data.label ]
        , div [ class "setting-picker-item-input setting-picker-item-input-buttons" ]
            (List.map
                (\{ label, onSelect, isSelected } ->
                    button
                        [ id "polarity-label"
                        , class ("setting-picker-item-button setting-picker-item-button-" ++ String.replace " " "-" label)
                        , classList [ ( "selected", isSelected ) ]
                        , onClick onSelect
                        ]
                        [ Html.text label ]
                )
                data.options
            )
        ]


getPlayerContainers : Game -> List (Html Msg)
getPlayerContainers game =
    Dict.values (Dict.map (\index playerData -> playerContainer index playerData game) game.players)


getPlayerScoreContainers : Game -> List (Html Msg)
getPlayerScoreContainers game =
    Dict.values (Dict.map (\index playerData -> playerScoreContainer index playerData) game.players)


getGameWinner : Game -> Int
getGameWinner game =
    let
        playerScores =
            getGameScore game
    in
    Dict.foldl
        (\player score ( aPlayer, aScore ) ->
            if score > aScore then
                ( player, score )

            else
                ( aPlayer, aScore )
        )
        ( -1, -1 )
        playerScores
        |> Tuple.first


getGameOverContainer : Game -> Html Msg
getGameOverContainer game =
    if game.status == GameOver then
        div [ id "game-over", class "game-over-container" ]
            [ div [ id "game-over-headers", class "game-over-header-container" ]
                [ h1 [ id "game-over-header" ] [ Html.text "Game Over!" ]
                , h1 [ id "game-over-subheader" ] [ Html.text "Winner: " ]
                , div [ id "player-numbers-row", class "player-number-row" ] [ playerNumContainer (getGameWinner game) ]
                ]
            , div [ id "game-over-scores", class "game-over-scores-container" ]
                [ h1 [ id "game-over-subheader" ] [ Html.text "Scores: " ]
                , div [ id "player-score-container", class "player-score-container" ] (getPlayerScoreContainers game)
                ]
            ]

    else
        div [] []


playerNumContainer : Int -> Html Msg
playerNumContainer playerNum =
    div [ id "num-container", class "player-number-container" ]
        [ div [ id "player-color", class "player-color-indicator" ]
            [ Svg.svg [ Svg.Attributes.viewBox "0 0 100 100", Svg.Attributes.width "40px", Svg.Attributes.height "40px" ]
                [ Svg.circle [ Svg.Attributes.cx "50%", Svg.Attributes.cy "50%", Svg.Attributes.r "50%", Svg.Attributes.fill (toCssString (playerColorToColor (getPlayerColor playerNum))) ] [] ]
            ]
        , h1 [ id "player-num", class "player-number" ]
            [ Html.text ("Player " ++ String.fromInt (playerNum + 1)) ]
        ]


playerScoreContainer : Int -> Player -> Html Msg
playerScoreContainer playerNum player =
    div [ id "player-score", class "player-score-container" ]
        [ playerNumContainer playerNum
        , div [ class "player-score-container" ]
            [ h2 [ id "score-box" ] [ Html.text ("Score: " ++ Round.round 3 player.score) ] ]
        ]


getPlayersOrGameOverContainer : Game -> Html Msg
getPlayersOrGameOverContainer game =
    if game.status == GameOver then
        getGameOverContainer game

    else
        div [ id "game-score-container", class "player-display-container" ] (getPlayerContainers game)


playerContainer : Int -> Player -> Game -> Html Msg
playerContainer playerNum player game =
    div [ id "player-info", class "player-container" ]
        [ playerNumContainer playerNum
        , div [ class "player-moves-container" ] [ div [ id "moves-num" ] [ Html.text ("Moves: " ++ String.fromInt player.remainingMoves) ] ]
        , viewPolaritySelector (polarityPickChoiceConfig playerNum game)

        --, div [ class "player-polarity-container" ] [ div [ id "polarity-selector" ] [ Html.text ("Polarity: " ++ toPolarityString player.polarity) ] ]
        , div [ class "player-score-box" ]
            [ div [ id "score-box" ] [ Html.text ("Score: " ++ Round.round 3 player.score) ] ]
        ]


view : Game -> Html Msg
view game =
    div [ id "game-screen-container" ]
        [ h1 [ id "game-header" ]
            [ Html.text "Magnet Melee!!!" ]
        , div [ id "turn-display" ] [ h2 [ id "turn-text" ] [ Html.text "Turn:    " ], playerNumContainer game.turn ]
        , div [ id "game-board", class "grid-container" ]
            [ getBoardView game
            , div [ id "game-scores-over", class "game-score-over-container" ]
                [ getPlayersOrGameOverContainer game
                ]
            ]
        ]
