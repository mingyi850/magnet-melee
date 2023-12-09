module SettingsComponents exposing (..)

{-| This module handles everything on the Settings screen.

TODO: You will need to modify this file to add / remove settings for your game.

Adding/removing a setting is a 5-step process.
(I know it seems like a lot, but it is necessary so Elm can make static
guarantees at compile time about your Settings).

I've outlined the five steps below under SETTING DEFINITIONS.

-}

import Array exposing (..)
import Common exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)



-- =============================================================================
-- =============================================================================
-- NOTE: YOU PROBABLY DON'T HAVE TO MODIFY ANYTHING BELOW THIS LINE
-- =============================================================================
-- =============================================================================
--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
-- If your use cases are covered by the basic types of settings above, you don't have to
-- edit any of the code below (it's boilerplate to make things easier for you).
-- However, if you need extra customisation, then you're welcome to edit it
-- if you know what you're doing (e.g. show a setting only in certain conditions,
-- or add extra specific styling to a setting).
--------------------------------------------------------------------------------
-- Helper functions to create Setting picker item types.
-- These are the functions you'll actually use to construct your pickers.
-- INPUT STRING


type alias InputStringConfig a =
    { label : String
    , value : String
    , onChange : String -> a
    }


{-| A basic text box that allows the user to input a string.
-}
inputString : InputStringConfig a -> SettingPickerItem a
inputString data =
    InputString data



-- INPUT FLOAT


type alias InputFloatConfig a =
    { label : String
    , value : Float
    , min : Float
    , max : Float
    , onChange : Float -> a
    }


{-| A basic box that allows the user to input a float.
-}
inputFloat : InputFloatConfig a -> SettingPickerItem a
inputFloat data =
    InputFloat data



-- INPUT INT


type alias InputIntConfig a =
    { label : String
    , value : Int
    , min : Int
    , max : Int
    , onChange : Int -> a
    }


{-| A basic box that allows the user to input an int.
-}
inputInt : InputIntConfig a -> SettingPickerItem a
inputInt data =
    InputInt data



-- INPUT FLOAT RANGE


type alias InputFloatRangeConfig a =
    { label : String
    , value : Float
    , step : Float
    , min : Float
    , max : Float
    , onChange : Float -> a
    }


{-| A range slider that allows the user to input a float.
-}
inputFloatRange : InputFloatRangeConfig a -> SettingPickerItem a
inputFloatRange data =
    InputFloatRange data



-- INPUT INT RANGE


type alias InputIntRangeConfig a =
    { label : String
    , value : Int
    , min : Int
    , max : Int
    , onChange : Int -> a
    }


{-| A range slider that allows the user to input an int.
-}
inputIntRange : InputIntRangeConfig a -> SettingPickerItem a
inputIntRange data =
    InputIntRange data



-- PICK CHOICE BUTTONS


type alias PickChoiceButtonsGenericConfig enum a =
    { label : String
    , onSelect : enum -> a
    , current : enum
    , options : List ( String, enum )
    }


{-| A set of buttons that allows the user to pick from a list of options.
-}
pickChoiceButtons : PickChoiceButtonsGenericConfig enum a -> SettingPickerItem a
pickChoiceButtons { label, onSelect, current, options } =
    PickChoiceButtons
        { label = label
        , options = List.map (\( optionLabel, value ) -> { label = optionLabel, onSelect = onSelect value, isSelected = value == current }) options
        }



-- PICK CHOICE DROPDOWN


type alias PickChoiceDropdownGenericConfig enum a =
    { label : String
    , onSelect : enum -> a
    , toString : enum -> String
    , fromString : String -> enum
    , current : enum
    , options : List ( String, enum )
    }


{-| A dropdown that allows the user to pick from a list of options.
-}
pickChoiceDropdown : PickChoiceDropdownGenericConfig enum a -> SettingPickerItem a
pickChoiceDropdown { label, onSelect, toString, fromString, current, options } =
    PickChoiceDropdown
        { label = label
        , onSelect = fromString >> onSelect
        , options = List.map (\( optionLabel, value ) -> { label = optionLabel, value = toString value, isSelected = value == current }) options
        }



--------------------------------------------------------------------------------
-- PICKER TYPES
--------------------------------------------------------------------------------


{-| A type of a single item in a setting picker

Note: these are NOT constructed directly. Instead, there are specific helper
functions to construct each of these. The reason is because Elm's type
system is a bit limited, and we want to be able to have different types of Enums
stored as items - so my compromise is to use more generic helper functions to convert it
into these types instead.

-}
type SettingPickerItem a
    = InputString (InputStringConfig a)
    | InputFloat (InputFloatConfig a)
    | InputInt (InputIntConfig a)
    | InputFloatRange (InputFloatRangeConfig a)
    | InputIntRange (InputIntRangeConfig a)
    | PickChoiceButtons (PickChoiceButtonsConfig a)
    | PickChoiceDropdown (PickChoiceDropdownConfig a)


type alias PickChoiceOptionButton a =
    { label : String
    , onSelect : a
    , isSelected : Bool
    }


type alias PickChoiceButtonsConfig a =
    { label : String
    , options : List (PickChoiceOptionButton a)
    }


type alias PickChoiceDropdownOption =
    { label : String
    , value : String
    , isSelected : Bool
    }


type alias PickChoiceDropdownConfig a =
    { label : String
    , onSelect : String -> a
    , options : List PickChoiceDropdownOption
    }



--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


{-| The view function for a single setting picker item.

Renders each item based on its type. You also have access to the
current settings in this function (as Settings) so can use that
information to make decisions on what to render as well.

-}
viewPickerItem : List (Html.Attribute a) -> SettingPickerItem a -> Html a
viewPickerItem attributes item =
    case item of
        InputString data ->
            div (class "setting-picker-item" :: attributes)
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , input [ class "setting-picker-item-input setting-picker-item-input-string", type_ "text", value data.value, onInput data.onChange ] []
                ]

        InputFloat data ->
            div (class "setting-picker-item" :: attributes)
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , input
                    [ class "setting-picker-item-input setting-picker-item-input-float"
                    , type_ "number"
                    , value (String.fromFloat data.value)
                    , Html.Attributes.min (String.fromFloat data.min)
                    , Html.Attributes.max (String.fromFloat data.max)
                    , onInput (String.toFloat >> Maybe.withDefault 0.0 >> data.onChange)
                    ]
                    []
                ]

        InputInt data ->
            div (class "setting-picker-item" :: attributes)
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , input
                    [ class "setting-picker-item-input setting-picker-item-input-int"
                    , type_ "number"
                    , value (String.fromInt data.value)
                    , Html.Attributes.min (String.fromInt data.min)
                    , Html.Attributes.max (String.fromInt data.max)
                    , onInput (String.toInt >> Maybe.withDefault 0 >> data.onChange)
                    ]
                    []
                ]

        InputFloatRange data ->
            div (class "setting-picker-item" :: attributes)
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , div [ class "setting-picker-item-input-container" ]
                    [ input
                        [ class "setting-picker-item-input setting-picker-item-input-float-range"
                        , type_ "range"
                        , value (String.fromFloat data.value)
                        , Html.Attributes.min (String.fromFloat data.min)
                        , Html.Attributes.max (String.fromFloat data.max)
                        , Html.Attributes.step (String.fromFloat data.step)
                        , onInput (String.toFloat >> Maybe.withDefault 0.0 >> data.onChange)
                        ]
                        []
                    , div [ class "setting-picker-item-input-value" ] [ text (String.fromFloat data.value) ]
                    ]
                ]

        InputIntRange data ->
            div (class "setting-picker-item" :: attributes)
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , div [ class "setting-picker-item-input-container" ]
                    [ input
                        [ class "setting-picker-item-input setting-picker-item-input-int-range"
                        , type_ "range"
                        , value (String.fromInt data.value)
                        , Html.Attributes.min (String.fromInt data.min)
                        , Html.Attributes.max (String.fromInt data.max)
                        , onInput (String.toInt >> Maybe.withDefault 0 >> data.onChange)
                        ]
                        []
                    , div [ class "setting-picker-item-input-value" ] [ text (String.fromInt data.value) ]
                    ]
                ]

        PickChoiceButtons data ->
            div (class "setting-picker-item" :: attributes)
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , div [ class "setting-picker-item-input setting-picker-item-input-buttons" ]
                    (List.map
                        (\{ label, onSelect, isSelected } ->
                            button
                                [ class ("setting-picker-item-button setting-picker-item-button-" ++ String.replace " " "-" label)
                                , classList [ ( "selected", isSelected ) ]
                                , onClick onSelect
                                ]
                                [ text label ]
                        )
                        data.options
                    )
                ]

        PickChoiceDropdown data ->
            div (class "setting-picker-item" :: attributes)
                [ label [ class "setting-picker-item-label" ] [ text data.label ]
                , select [ class "setting-picker-item-input setting-picker-item-input-select", onInput data.onSelect ]
                    (List.map
                        (\optionData ->
                            option [ value optionData.value, selected optionData.isSelected ] [ text optionData.label ]
                        )
                        data.options
                    )
                ]
