module Physics exposing (..)

import Utils exposing (..)
import VectorUtils exposing (..)


type alias PVector =
    { x : Float
    , y : Float
    }


type alias PCoordinate =
    { x : Float
    , y : Float
    }


calculateForceVector : Int -> PCoordinate -> PCoordinate -> ( PVector, Float )
calculateForceVector magnetism coordinate pieceCoordinate =
    let
        eDistance =
            euclideanDistance coordinate pieceCoordinate

        distanceVector =
            { x = coordinate.x - pieceCoordinate.x, y = coordinate.y - pieceCoordinate.y }

        distanceVectorSum =
            abs distanceVector.x + abs distanceVector.y

        magnetStrength =
            if eDistance == 0 then
                0

            else
                toFloat magnetism / (eDistance ^ 2)

        resultantVector =
            { x = distanceVector.x * magnetStrength / distanceVectorSum
            , y = distanceVector.y * magnetStrength / distanceVectorSum
            }
    in
    ( resultantVector, magnetStrength )


getFrictionVector : Float -> PVector -> PVector
getFrictionVector friction vector =
    let
        xMagnitude =
            abs vector.x

        yMagnitude =
            abs vector.y

        xFriction =
            friction * xMagnitude

        yFriction =
            friction * yMagnitude

        xSign =
            if vector.x < 0 then
                1

            else
                -1

        ySign =
            if vector.y < 0 then
                1

            else
                -1
    in
    { x = xFriction * xSign, y = yFriction * ySign }


checkVectorStop : PVector -> PVector
checkVectorStop vector =
    if sqrt (vector.x ^ 2 + vector.y ^ 2) < 1 then
        { x = 0, y = 0 }

    else
        vector


calculateCollisionVector : ( PCoordinate, PVector ) -> ( PCoordinate, PVector ) -> ( PVector, PVector )
calculateCollisionVector ( c1, v1 ) ( c2, v2 ) =
    let
        proportion =
            vectorMagnitude v1 / (vectorMagnitude v1 + vectorMagnitude v2)

        impulse =
            calculateImpulse 1 v1 v2 (calculateCollisionNormal c1 c2)

        antiV1 =
            multiplyVector proportion impulse

        antiV2 =
            multiplyVector (1 - proportion) (negative impulse)
    in
    ( antiV1, antiV2 )


calculateImpulse : Float -> PVector -> PVector -> PVector -> PVector
calculateImpulse restitution velocity1 velocity2 collisionNormal =
    let
        relativeVelocity =
            { x = velocity1.x - velocity2.x, y = velocity1.y - velocity2.y }

        velocityScaleAlongNormal =
            relativeVelocity.x * collisionNormal.x + relativeVelocity.y * collisionNormal.y

        velocityAlongNormalVector =
            { x = restitution * velocityScaleAlongNormal * collisionNormal.x, y = restitution * velocityScaleAlongNormal * collisionNormal.y }

        impulse =
            if velocityScaleAlongNormal > 0 then
                negative velocityAlongNormalVector

            else
                { x = 0, y = 0 }
    in
    impulse


calculateCollisionNormal : PCoordinate -> PCoordinate -> FloatVector
calculateCollisionNormal position1 position2 =
    let
        directionVector =
            { x = position2.x - position1.x, y = position2.y - position1.y }

        magnitude =
            sqrt (directionVector.x ^ 2 + directionVector.y ^ 2)

        collisionNormal =
            { x = directionVector.x / magnitude, y = directionVector.y / magnitude }
    in
    collisionNormal
