module Main exposing (..)

{-| This is the main entrypoint for your game in Elm.

Essentially, this file does the routing to the Settings screen or the Gameplay
screen.

-}

import Browser
import Common exposing (..)
import Game exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Settings exposing (..)
import Task exposing (..)
import Time exposing (..)



--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


{-| The application shows either the Settings screen or the Gameplay screen.

Each of these screens has their own separate model (although when you start
the Gameplay screen, all the current settings given to it are saved into
part of its model).

-}
type Model
    = SettingsScreen Settings
    | GameplayScreen Game


{-| The initial model for the application (when it starts) is the Settings
screen with the default settings.
-}
init : ( Model, Cmd Msg )
init =
    ( SettingsScreen Settings.default, Cmd.none )



--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------


{-| The application can receive messages from the Settings screen or the
Gameplay screen. This function routes those messages to the appropriate
update function for the screen they came from.

There are also two messages that allow transitions between screens:

  - ClickedStartGame (Settings screen => Gameplay screen)
  - ClickedRestart (Gamplay screen => Settings screen)

These are the only two ways to communicate between the Settings screen
and Gameplay screen.

-}
type Msg
    = SettingsMsg Settings.Msg
    | GameplayMsg Game.Msg
    | ClickedStartGame
    | ClickedRestart
    | Tick Posix
    | StartGameNow Posix


{-| Helper function to allow piping of a Cmd Msg into a tuple with a Model.
-}
withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCmd cmd screen =
    ( screen, cmd )


{-| Helper function to lift the Game Model and Msg into this file's Main model.
-}
mapGameCmd : ( Game, Cmd Game.Msg ) -> ( Model, Cmd Msg )
mapGameCmd ( game, cmd ) =
    ( GameplayScreen game, Cmd.map GameplayMsg cmd )


{-| The update function for the application. This function routes messages
to the appropriate update function for the screen they came from, and also handles
the two messages that allow transitions between screens.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg screen =
    case screen of
        SettingsScreen settings ->
            case msg of
                -- If we get a Settings message, we update the Settings screen as per the update function in Settings.elm.
                SettingsMsg settingsMsg ->
                    SettingsScreen (Settings.update settingsMsg settings)
                        |> withCmd Cmd.none

                -- When the user clicks Start Game, we initialize a new Game with the current settings.
                ClickedStartGame ->
                    SettingsScreen settings
                        |> withCmd (Task.perform StartGameNow Time.now)

                StartGameNow posix ->
                    Game.init { settings | time = posixToMillis posix }
                        |> mapGameCmd

                _ ->
                    screen
                        |> withCmd Cmd.none

        GameplayScreen game ->
            case msg of
                GameplayMsg gameMsg ->
                    Game.update gameMsg game
                        |> mapGameCmd

                ClickedRestart ->
                    SettingsScreen game.settings
                        |> withCmd Cmd.none

                _ ->
                    screen
                        |> withCmd Cmd.none



--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------


introText : Html Msg
introText =
    div [ class "intro-text" ]
        [ h3 [] [ text "Magnet Melee! Control as much space on the board as possible using your magnetic powers." ]
        , p []
            [ strong [] [ text "Magnetism: " ]
            , text "Strength of each magnet - the stronger the magnet, the more force they exert on other magnets."
            ]
        , p []
            [ strong [] [ text "Friction: " ]
            , text "Amount of friction on the field - this controls how much speed each magnet loses as it moves"
            ]
        ]


view : Model -> Html Msg
view screen =
    case screen of
        SettingsScreen settings ->
            div [ id "settings-screen", class "screen" ]
                [ div [ id "settings-modal" ]
                    [ div [ id "settings-modal-header" ]
                        [ h1 [ id "settings-modal-header-title" ] [ text "Magnet Melee" ]
                        , h2 [ id "settings-modal-header-team" ] [ text "Mingyi Lim" ]
                        ]
                    , div [ id "settings-modal-intro" ] [ introText ]
                    , div [ id "settings-modal-body" ] [ Settings.view settings |> Html.map SettingsMsg ]
                    , div [ id "settings-modal-footer" ] [ button [ id "start-game-button", onClick ClickedStartGame ] [ text "Start Game" ] ]
                    ]
                ]

        GameplayScreen game ->
            div [ id "gameplay-screen", class "screen" ]
                [ Game.view game |> Html.map GameplayMsg
                , div [ id "restart-button-container" ] [ button [ id "restart-button", onClick ClickedRestart ] [ text "Restart" ] ]
                ]



--------------------------------------------------------------------------------
-- PROGRAM
--------------------------------------------------------------------------------


{-| The actual main entrypoint to run the application.
-}
main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
