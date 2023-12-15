module Game exposing (..)

{-| This file handles all the game logic and provides the Gameplay interface to the Main application.alias.
-}

import Array exposing (..)
import BoardCellGrid exposing (InteractionType(..), Msg)
import CellGrid exposing (..)
import Common exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Models exposing (..)
import Process
import Random exposing (..)
import Round exposing (..)
import Settings exposing (..)
import SettingsComponents exposing (..)
import Svg exposing (..)
import Svg.Attributes
import Task
import Utils.ColorUtils exposing (..)
import Utils.Utils exposing (..)



--------------------------------------------------------------------------------
-- GAME MODEL
--------------------------------------------------------------------------------


{-| A record type which contains all of the game state.
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
    , randomMove : RandomMove
    , friction : Float
    , padding : Int
    , aiMoves : Int
    , aiProgress : Int
    }


type alias GameMoveResult =
    { status : Status
    , game : Game
    }


type alias RandomMove =
    { move : MoveData
    , seed : Seed
    , score : Maybe Float
    }


type Agent
    = Human
    | AIEasy
    | AIHard


getAgentFromInt : Int -> Agent
getAgentFromInt agent =
    case agent of
        0 ->
            Human

        1 ->
            AIEasy

        2 ->
            AIHard

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
            , status = getInitGameStatus settings
            , randomMove = { move = { x = settings.gridSize // 2, y = settings.gridSize // 2, polarity = Negative }, seed = initialSeed settings.time, score = Nothing }
            , friction = settings.friction
            , padding = settings.padding
            , aiMoves = Basics.max 150 settings.gridSize * 2
            , aiProgress = 0
            }
    in
    initialGame |> (\game -> withDetermineUpdateCommand game [] game)


{-| Checks which players turn it is to start the game.
-}
getInitGameStatus : Settings -> GameStatus
getInitGameStatus settings =
    let
        firstPlayerAgency =
            getAgentFromInt (Maybe.withDefault 0 (Array.get 0 settings.playerAI))
    in
    case firstPlayerAgency of
        Human ->
            HumanMove

        AIEasy ->
            AI 0

        AIHard ->
            AI 1



--------------------------------------------------------------------------------
-- GAME LOGIC
--------------------------------------------------------------------------------


{-| The possible moves that a player can make.
-}
type Move
    = SelectPolarity Int Polarity
    | PlacePiece IntCoordinate Polarity
    | DisplayPiece IntCoordinate Polarity


{-| Status of a given move
-}
type Status
    = Success
    | Failure


{-| Model for a move users can make
-}
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
            if checkValidPiecePlacement (intCoordinateToFloat coordinate) Nothing game.board then
                { status = Success
                , game =
                    { game
                        | board =
                            removeTentativePieces game.board
                                |> insertPiece { player = game.turn, polarity = polarity } (intCoordinateToFloat coordinate)
                                |> updateBoardMagneticField game.magnetism
                    }
                }

            else
                { status = Failure, game = game }

        DisplayPiece coordinate polarity ->
            if checkValidPiecePlacement (intCoordinateToFloat coordinate) Nothing game.board then
                { status = Success, game = { game | board = insertTentativePiece { player = game.turn, polarity = polarity } (intCoordinateToFloat coordinate) game.board |> updateBoardMagneticField game.magnetism } }

            else
                { status = Failure, game = { game | board = removeTentativePieces game.board |> updateBoardMagneticField game.magnetism } }



{-------------------------------------------
    Update methods
-------------------------------------------}


{-| Calls board API to get board score
-}
updatePlayerScores : Dict Int Player -> Dict Int Float -> Dict Int Player
updatePlayerScores players scoreDict =
    Dict.map
        (\player playerData ->
            { playerData
                | score = Dict.get player scoreDict |> Maybe.withDefault 0.0
            }
        )
        players


{-| Updates the state of the game board
-}
updateGameBoard : Game -> Game
updateGameBoard game =
    let
        gameScores =
            getGameScore game

        newGame =
            { game
                | board = updateBoardMagneticField game.magnetism (updatePiecePositions (updatePieceVelocities game.friction game.magnetism game.board))
                , players = updatePlayerScores game.players gameScores
            }
    in
    if newGame.board.pieceCoordinates == game.board.pieceCoordinates then
        case game.status of
            Processing ->
                { newGame | status = getGameStatus game, board = zeroPieceVelocities newGame.board }

            _ ->
                { newGame | board = zeroPieceVelocities newGame.board }

    else
        newGame


{-| Updates the polarity of a player from the game screen
-}
updatePlayerPolarity : Int -> Polarity -> Game -> Game
updatePlayerPolarity player polarity game =
    { game
        | players =
            Dict.update player (Maybe.map (\playerData -> { playerData | polarity = polarity })) game.players
    }



{-------------------------------------------
    Getter methods to query game state 
-------------------------------------------}


{-| Gets the current game score
-}
getGameScore : Game -> Dict Int Float
getGameScore game =
    let
        scores =
            Dict.fromList (range 0 (Dict.size game.players - 1) |> List.map (\player -> ( player, 0.0 )))

        boardScores =
            getBoardScores game.board
    in
    addDicts scores boardScores


{-| Gets a given player's polarity
-}
getPlayerPolarity : Int -> Game -> Polarity
getPlayerPolarity player game =
    Maybe.withDefault Negative (Maybe.map (\playerData -> playerData.polarity) (Dict.get player game.players))


{-| Gets a given player's agency (AI or Human)
-}
getPlayerAgency : Int -> Game -> Agent
getPlayerAgency player game =
    Maybe.withDefault Human (Maybe.map (\playerData -> playerData.agent) (Dict.get player game.players))


{-| Get current game status
-}
getGameStatus : Game -> GameStatus
getGameStatus game =
    if checkGameOver game then
        GameOver

    else
        case getPlayerAgency game.turn game of
            Human ->
                HumanMove

            AIEasy ->
                AI 0

            AIHard ->
                AI 1


{-| Checks game over condition
-}
checkGameOver : Game -> Bool
checkGameOver game =
    if Dict.foldl (\_ playerData accum -> (playerData.remainingMoves <= 0) && accum) True game.players then
        True

    else
        False



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
    | GenerateAIMove Int
    | PlayAIMove
    | UpdateBoard (List Board)
    | UpdatePlayerPolarity Int Polarity


{-| A convenience function to pipe a command into a (Game, Cmd Msg) tuple.
-}
withCmd : Cmd Msg -> Game -> ( Game, Cmd Msg )
withCmd cmd game =
    ( game, cmd )


{-| Sends a message to the Elm runtime with some delay
-}
send : Float -> msg -> Cmd msg
send wait msg =
    Process.sleep wait
        |> Task.perform (\_ -> msg)


{-| Sends a message to the Elm runtime without delay
-}
sendNow : msg -> Cmd msg
sendNow msg =
    Task.succeed msg
        |> Task.perform identity


{-| Sends a message to the Elm runtime to fetch a move from the AI
-}
sendGetAIMove : Int -> Game -> ( Game, Cmd Msg )
sendGetAIMove player game =
    ( game, send 200.0 (GenerateAIMove 1) )


{-| Utility function to always send a specific message regardless of game move status
-}
alwaysCommand : Cmd Msg -> GameMoveResult -> ( Game, Cmd Msg )
alwaysCommand msg { game, status } =
    ( game, msg )


{-| Attaches update command to game based on game state
-}
withDetermineUpdateCommand : Game -> List Board -> Game -> ( Game, Cmd Msg )
withDetermineUpdateCommand prevGame previousStates game =
    withCmd (determineUpdateCommand prevGame previousStates game) game


{-| Determines update c ommand based on game state
-}
determineUpdateCommand : Game -> List Board -> Game -> Cmd Msg
determineUpdateCommand previousGame previousStates game =
    case game.status of
        Processing ->
            send 50.0 (UpdateBoard (List.append previousStates [ previousGame.board ]))

        AI 0 ->
            send 100.0 (GenerateAIMove 0)

        AI 1 ->
            send 100.0 (GenerateAIMove game.aiMoves)

        _ ->
            Cmd.none


{-| Get Cell Coordinates from CellGrid Click
-}
determineCellCoordinates : Game -> BoardCellGrid.Msg -> Maybe IntCoordinate
determineCellCoordinates game cellMsg =
    let
        boardCoordinate =
            mapViewCoordinateToBoard game.padding game.gridSize { x = cellMsg.cell.column, y = cellMsg.cell.row }
    in
    boardCoordinate


{-| Utility Function to used to decide next action based on game state
-}
processMoveResult : Int -> (Int -> Game -> ( Game, Cmd Msg )) -> (Int -> Game -> ( Game, Cmd Msg )) -> GameMoveResult -> ( Game, Cmd Msg )
processMoveResult player onSuccess onFailure moveResult =
    case moveResult.status of
        Success ->
            onSuccess player moveResult.game

        Failure ->
            onFailure player moveResult.game


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
                                    case determineCellCoordinates game cellmsg of
                                        Just coordinate ->
                                            game
                                                |> applyMove (PlacePiece coordinate (getPlayerPolarity game.turn game))
                                                |> processMoveResult game.turn progressGameSuccess (\_ _ -> ( game, Cmd.none ))

                                        Nothing ->
                                            ( game, Cmd.none )

                                Hover ->
                                    case determineCellCoordinates game cellmsg of
                                        Just coordinate ->
                                            game
                                                |> applyMove (DisplayPiece coordinate (getPlayerPolarity game.turn game))
                                                |> alwaysCommand Cmd.none

                                        Nothing ->
                                            ( game, Cmd.none )

                        _ ->
                            ( game, Cmd.none )

        UpdateBoard previousStates ->
            updateGameBoard game
                |> withDetermineUpdateCommand game previousStates

        GenerateAIMove num ->
            case game.status of
                AI 0 ->
                    let
                        newMove =
                            generateRandomMove game
                    in
                    case newMove.score of
                        Just _ ->
                            update PlayAIMove { game | randomMove = newMove }

                        Nothing ->
                            { game | randomMove = newMove }
                                |> withCmd (sendNow (GenerateAIMove 0))

                AI 1 ->
                    if num == 0 then
                        game
                            |> withCmd (sendNow PlayAIMove)

                    else
                        let
                            newMove =
                                generateRandomMove game

                            maybeNewMoveScore =
                                getMoveScore game.turn newMove.move game
                        in
                        case maybeNewMoveScore of
                            Just newMoveScore ->
                                case game.randomMove.score of
                                    Just score ->
                                        if newMoveScore > score then
                                            { game | randomMove = newMove, aiProgress = game.aiMoves - num }
                                                |> withCmd (send 0.0 (GenerateAIMove (num - 1)))

                                        else
                                            { game | randomMove = { move = game.randomMove.move, score = game.randomMove.score, seed = newMove.seed }, aiProgress = game.aiMoves - num }
                                                |> withCmd (send 0.0 (GenerateAIMove (num - 1)))

                                    Nothing ->
                                        { game | randomMove = newMove, aiProgress = game.aiMoves - num }
                                            |> withCmd (send 0.0 (GenerateAIMove (num - 1)))

                            Nothing ->
                                update (GenerateAIMove num) { game | randomMove = newMove, aiProgress = game.aiMoves - num }

                _ ->
                    ( game, Cmd.none )

        PlayAIMove ->
            case game.status of
                AI _ ->
                    let
                        aiMove =
                            game.randomMove.move
                    in
                    applyMove (PlacePiece { x = aiMove.x, y = aiMove.y } aiMove.polarity) game
                        |> processMoveResult game.turn progressGameSuccess sendGetAIMove

                _ ->
                    ( game, Cmd.none )


{-| Progress game state after a successful move
-}
progressGameSuccess : Int -> Game -> ( Game, Cmd Msg )
progressGameSuccess player game =
    { game
        | players =
            Dict.update player (Maybe.map (\playerData -> { playerData | remainingMoves = playerData.remainingMoves - 1 })) game.players
        , totalMoves = game.totalMoves + 1
        , turn = modBy (Dict.size game.players) (game.totalMoves + 1)
        , status = Processing
        , randomMove = { move = game.randomMove.move, seed = game.randomMove.seed, score = Nothing }
    }
        |> withCmd (send 200.0 (UpdateBoard []))



{------------------------------------------------------------------------------
    AI Helper Functions
------------------------------------------------------------------------------}


{-| Simulates the game board for a given number of steps
-}
simulateGameBoard : Int -> Game -> Game -> Game
simulateGameBoard steps prevGame currentGame =
    if steps <= 0 || prevGame.board.coordinatePieces == currentGame.board.coordinatePieces then
        currentGame

    else
        simulateGameBoard (steps - 1) currentGame (updateGameBoard currentGame)


{-| Utility function to evaluate an AI's move
-}
getMoveScore : Int -> MoveData -> Game -> Maybe Float
getMoveScore player move game =
    if checkValidPiecePlacement (intCoordinateToFloat { x = move.x, y = move.y }) Nothing game.board then
        let
            newGame =
                { game | board = insertPiece { player = player, polarity = move.polarity } (intCoordinateToFloat { x = move.x, y = move.y }) game.board }

            simulatedGame =
                simulateGameBoard 10 game newGame
        in
        Just (getAvgScoreMargin player simulatedGame)

    else
        Nothing


{-| Gets the average difference in score between a player and other players
-}
getAvgScoreMargin : Int -> Game -> Float
getAvgScoreMargin player game =
    let
        scores =
            getGameScore game

        playerScore =
            Dict.get player scores |> Maybe.withDefault 0.0
    in
    Dict.values scores |> List.map (\otherScore -> playerScore - otherScore) |> List.foldl (+) 0.0 |> (\x -> x / toFloat (Dict.size scores - 1))


generateRandomMove : Game -> RandomMove
generateRandomMove game =
    let
        randomCoordinate =
            Random.step (Random.pair (Random.int 0 (game.board.config.gridDimensions - 1)) (Random.int 0 (game.board.config.gridDimensions - 1))) game.randomMove.seed
                |> (\( ( x, y ), seed ) -> { x = x, y = y, seed = seed })

        randomPolarity =
            if Tuple.first (Random.step (Random.int 0 1) randomCoordinate.seed) == 0 then
                Negative

            else
                Positive

        move =
            { x = randomCoordinate.x, y = randomCoordinate.y, polarity = randomPolarity }
    in
    { move = move, seed = randomCoordinate.seed, score = getMoveScore game.turn move game }



--------------------------------------------------------------------------------
-- GAME VIEW FUNCTION
--------------------------------------------------------------------------------


{-| The main view function that gets called from the Main application.

Essentially, takes a game and projects it into a HTML interface where Messages
can be sent from.

-}
getBoardConfig : Settings -> BoardConfig
getBoardConfig settings =
    let
        padding =
            settings.padding

        gridSize =
            settings.gridSize

        cellSize =
            600 // settings.gridSize
    in
    { displaySize = cellSize * (gridSize + padding * 2)
    , gridDimensions = settings.gridSize
    , padding = settings.padding
    }


getBoardView : Game -> Html Msg
getBoardView game =
    Html.map ModelMsg
        (div [ id "board-container", class "game-board" ]
            [ boardHtml game.padding game.magnetism game.board ]
        )


polarityPickChoiceConfig : Int -> Game -> PickChoiceButtonsConfig Msg
polarityPickChoiceConfig player game =
    { label = "Polarity"
    , options =
        [ { label = "+", onSelect = UpdatePlayerPolarity player Positive, isSelected = getPlayerPolarity player game == Positive }
        , { label = "-", onSelect = UpdatePlayerPolarity player Negative, isSelected = getPlayerPolarity player game == Negative }
        ]
    }


viewPolaritySelector : Game -> PickChoiceButtonsConfig Msg -> Html Msg
viewPolaritySelector game data =
    viewPickerItem "polarity-selector" [ Html.Attributes.style "font-size" (px (24 - (2 * Dict.size game.players))), Html.Attributes.style "color" "#ccc" ] (PickChoiceButtons data)


getPlayerContainers : Game -> List (Html Msg)
getPlayerContainers game =
    Dict.values (Dict.map (\index playerData -> playerContainer index playerData game) game.players)


getPlayerScoreContainers : Game -> List (Html Msg)
getPlayerScoreContainers game =
    Dict.values (Dict.map (\index playerData -> playerScoreContainer game index playerData) game.players)


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
                [ h1 [ id "game-over-subheader" ] [ Html.text "Winner: " ]
                , div [ id "player-numbers-row", class "player-number-row" ] [ playerNumContainer False game (getGameWinner game) ]
                ]
            , div [ id "game-over-scores", class "game-over-scores-container" ]
                [ h1 [ id "game-over-subheader" ] [ Html.text "Scores: " ]
                , div [ id "player-score-container", class "player-score-container" ] (getPlayerScoreContainers game)
                ]
            ]

    else
        div [] []


getClockLoader : Game -> Html Msg
getClockLoader game =
    let
        progress =
            game.aiProgress * 100 // game.aiMoves
    in
    if progress < 25 then
        span [ id "clock-loader", class "material-symbols-outlined" ] [ Html.text "clock_loader_10" ]

    else if progress < 33 then
        span [ id "clock-loader", class "material-symbols-outlined" ] [ Html.text "clock_loader_20" ]

    else if progress < 66 then
        span [ id "clock-loader", class "material-symbols-outlined" ] [ Html.text "clock_loader_40" ]

    else if progress < 75 then
        span [ id "clock-loader", class "material-symbols-outlined" ] [ Html.text "clock_loader_60" ]

    else if progress < 90 then
        span [ id "clock-loader", class "material-symbols-outlined" ] [ Html.text "clock_loader_80" ]

    else
        span [ id "clock-loader", class "material-symbols-outlined" ] [ Html.text "clock_loader_90" ]


playerNumContainer : Bool -> Game -> Int -> Html Msg
playerNumContainer isHeader game playerNum =
    let
        playerAgent =
            case getPlayerAgency playerNum game of
                Human ->
                    ""

                AIEasy ->
                    "(AI)"

                AIHard ->
                    "(AI)"

        size =
            if isHeader then
                px 30

            else
                px (55 - (10 * Dict.size game.players))

        textSize =
            if isHeader then
                px 24

            else
                px (24 - (2 * Dict.size game.players))
    in
    div [ id "num-container", class "player-number-container" ]
        [ div [ id "player-color", class "player-color-indicator", Svg.Attributes.width size ]
            [ Svg.svg [ Svg.Attributes.viewBox "0 0 100 100", Svg.Attributes.width size, Svg.Attributes.height size ]
                [ Svg.circle
                    [ Svg.Attributes.cx "50%"
                    , Svg.Attributes.cy "50%"
                    , Svg.Attributes.r "45%"
                    , Svg.Attributes.fill (toCssString (getPieceColor { player = playerNum, polarity = None }))
                    , Svg.Attributes.stroke "#777"
                    , Svg.Attributes.strokeWidth "5"
                    ]
                    []
                ]
            ]
        , h1 [ id "player-num", class "player-number", Html.Attributes.style "font-size" textSize ]
            [ Html.text ("Player " ++ String.fromInt (playerNum + 1) ++ playerAgent) ]
        ]


playerScoreContainer : Game -> Int -> Player -> Html Msg
playerScoreContainer game playerNum player =
    div [ id "player-score", class "player-score-container" ]
        [ playerNumContainer False game playerNum
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
        [ playerNumContainer False game playerNum
        , viewPolaritySelector game (polarityPickChoiceConfig playerNum game)
        , div [ class "player-moves-container", Html.Attributes.style "font-size" (px (24 - (2 * Dict.size game.players))) ] [ div [ id "moves-num" ] [ Html.text ("Moves: " ++ String.fromInt player.remainingMoves) ] ]
        , div [ class "player-score-box" ]
            [ div [ id "score-box", Html.Attributes.style "font-size" (px (24 - (2 * Dict.size game.players))) ] [ Html.text ("Score: " ++ Round.round 3 player.score) ] ]
        ]


getTurnDisplay : GameStatus -> Game -> Html Msg
getTurnDisplay status game =
    case status of
        HumanMove ->
            div [ id "turn-display" ] [ h3 [ id "turn-text" ] [ Html.text "Turn:    " ], playerNumContainer True game game.turn ]

        AI _ ->
            div [ id "turn-display" ] [ h3 [ id "turn-text" ] [ Html.text "Turn:    " ], playerNumContainer True game game.turn, Html.text "Thinking", getClockLoader game ]

        Processing ->
            div [ id "turn-display" ] [ h3 [ id "turn-text" ] [ Html.text "Processing   " ] ]

        GameOver ->
            div [ id "turn-display" ] [ h3 [ id "turn-text" ] [ Html.text "Game Over   " ] ]


view : Game -> Html Msg
view game =
    let
        backgroundCol =
            case game.status of
                GameOver ->
                    "#333"

                _ ->
                    "#333"
    in
    div [ id "game-screen-container", Html.Attributes.style "background-color" backgroundCol, Html.Attributes.style "color" "#ccc" ]
        [ getTurnDisplay game.status game
        , div [ id "game-board", class "grid-container" ]
            [ getBoardView game
            , div [ id "game-scores-over", class "game-score-over-container" ]
                [ getPlayersOrGameOverContainer game
                ]
            ]
        , span [ id "clock-loader", class "material-symbols-outlined", Html.Attributes.style "font-size" "0px" ] [ Html.text "clock_loader_10" ]
        ]
