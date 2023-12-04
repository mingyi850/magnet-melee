module Utils exposing (..)

import Dict exposing (..)


addDicts : Dict Int Float -> Dict Int Float -> Dict Int Float
addDicts dict1 dict2 =
    Dict.foldl (\key value existing -> Dict.update key (\existingValue -> Maybe.map ((+) value) existingValue) existing) dict1 dict2
