module Settings exposing (..)

{-| This module handles everything on the Settings screen.
-}

import Array exposing (..)
import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import SettingsComponents exposing (..)


type alias Settings =
    { gridSize : Int
    , magnetism : Int
    , friction : Float
    , maxMoves : Int
    , players : Int
    , playerAI : Array Int
    , time : Int
    , padding : Int
    }


default : Settings
default =
    { gridSize = 50
    , magnetism = 50
    , friction = 0.3
    , maxMoves = 5
    , players = 2
    , playerAI = Array.repeat 2 0
    , time = 0
    , padding = 3
    }


type Msg
    = SetGridSize Int
    | SetMagnetism Int
    | SetFriction Float
    | SetNumMoves Int
    | SetNumPlayers Int
    | SetPlayerAi Int Int


{-| Settings updates functions - these are called from the Main application.
-}
update : Msg -> Settings -> Settings
update msg settings =
    case msg of
        SetGridSize size ->
            { settings | gridSize = size, magnetism = getMagnetismForGridSize settings.gridSize size settings.magnetism }

        SetMagnetism magnetism ->
            { settings | magnetism = magnetism }

        SetFriction friction ->
            { settings | friction = friction }

        SetNumMoves moves ->
            { settings | maxMoves = moves }

        SetNumPlayers players ->
            { settings | players = players, playerAI = Array.repeat players 0 }

        SetPlayerAi player ai ->
            { settings | playerAI = Array.set player ai settings.playerAI }


getMagnetismForGridSize : Int -> Int -> Int -> Int
getMagnetismForGridSize oldGridSize newGridSize oldMagnetism =
    let
        magnetismRatio =
            toFloat oldMagnetism / toFloat oldGridSize
    in
    round (toFloat newGridSize * magnetismRatio)


pickers : Settings -> List (SettingPickerItem Msg)
pickers settings =
    [ inputIntRange
        { label = "Number of Moves"
        , value = settings.maxMoves
        , min = 1
        , max = 30
        , onChange = SetNumMoves
        }
    , pickChoiceButtons
        { label = "GridSize"
        , current = settings.gridSize
        , options = [ ( "Small", 25 ), ( "Medium", 50 ), ( "Large", 100 ) ]
        , onSelect = SetGridSize
        }
    , pickChoiceButtons
        { label = "Magnetism"
        , current = settings.magnetism
        , options = [ ( "Weak", settings.gridSize // 2 ), ( "Medium", settings.gridSize ), ( "Strong", round (toFloat settings.gridSize * 1.5) ), ( "XtraStrong", round (toFloat settings.gridSize * 2) ) ]
        , onSelect = SetMagnetism
        }
    , pickChoiceButtons
        { label = "Friction"
        , current = settings.friction
        , options = [ ( "0.1", 0.1 ), ( "0.3", 0.3 ), ( "0.5", 0.5 ), ( "0.7", 0.7 ), ( "0.9", 0.9 ) ]
        , onSelect = SetFriction
        }
    , pickChoiceButtons
        { label = "Number of Players"
        , onSelect = SetNumPlayers
        , current = settings.players
        , options = [ ( "2", 2 ), ( "3", 3 ), ( "4", 4 ) ]
        }
    ]


aiPickers : Settings -> List (SettingPickerItem Msg)
aiPickers settings =
    List.map
        (\player ->
            pickChoiceButtons
                { label = "Player " ++ String.fromInt (player + 1)
                , onSelect = SetPlayerAi player
                , current = Maybe.withDefault 0 (Array.get player settings.playerAI)
                , options = [ ( "Human", 0 ), ( "Easy", 1 ), ( "Hard", 2 ) ]
                }
        )
        (List.range 0 (settings.players - 1))


{-| View just the picker part of the settings
-}
viewPicker : List (Html.Attribute Msg) -> String -> List (SettingPickerItem Msg) -> Html Msg
viewPicker attributes name items =
    div [ id ("settings-picker-" ++ name) ]
        (List.map (viewPickerItem name attributes) items)


{-| The function that views all settings which gets called from the Main application.
-}
view : Settings -> Html Msg
view settings =
    div [ id "settings" ]
        [ viewPicker []
            "main"
            (pickers settings)
        , viewPicker [] "ai" (aiPickers settings)
        ]
