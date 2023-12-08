module VectorUtils exposing (..)


type alias FloatVector =
    { x : Float
    , y : Float
    }


type alias IntVector =
    { x : Int
    , y : Int
    }


negative : FloatVector -> FloatVector
negative vector =
    { x = -vector.x, y = -vector.y }


unit : FloatVector -> IntVector
unit vector =
    let
        x =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                1

            else
                -1

        y =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                1

            else
                -1
    in
    { x = x, y = y }


multiplyVector : Float -> FloatVector -> FloatVector
multiplyVector scale vector =
    { x = vector.x * scale, y = vector.y * scale }


scaledUnit : Int -> FloatVector -> IntVector
scaledUnit scale vector =
    let
        x =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                scale

            else
                -scale

        y =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                scale

            else
                -scale
    in
    { x = x, y = y }


scaledIntUnit : Int -> IntVector -> IntVector
scaledIntUnit scale vector =
    let
        x =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                scale

            else
                -scale

        y =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                scale

            else
                -scale
    in
    { x = x, y = y }


unitIntVector : IntVector -> IntVector
unitIntVector vector =
    let
        x =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                1

            else
                -1

        y =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                1

            else
                -1
    in
    { x = x, y = y }


combineVectors : FloatVector -> FloatVector -> FloatVector
combineVectors vector1 vector2 =
    { x = vector1.x + vector2.x, y = vector1.y + vector2.y }


increaseFloatVectorMagnitude : FloatVector -> FloatVector
increaseFloatVectorMagnitude vector =
    let
        newX =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                vector.x + 1

            else
                vector.x - 1

        newY =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                vector.y + 1

            else
                vector.y - 1
    in
    { x = newX, y = newY }


decreaseIntVectorMagnitude : IntVector -> IntVector
decreaseIntVectorMagnitude vector =
    let
        newX =
            if vector.x == 0 then
                0

            else if vector.x > 0 then
                vector.x - 1

            else
                vector.x + 1

        newY =
            if vector.y == 0 then
                0

            else if vector.y > 0 then
                vector.y - 1

            else
                vector.y + 1
    in
    { x = newX, y = newY }
