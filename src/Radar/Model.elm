module Radar.Model exposing (Blip, Quadrant, Radar, Ring, circleBlip, triangleBlip)

import Random exposing (Generator)
import Svg exposing (Svg, path)
import Svg.Attributes exposing (d, transform)


type alias Radar =
    List Blip


type alias Blip =
    { name : String
    , ring : Ring
    , quadrant : Quadrant
    , isNew : Bool
    , description : String
    }


type Ring
    = Hold
    | Assess
    | Trial
    | Adopt


type Quadrant
    = Tools
    | Techniques
    | Platforms
    | LangsAndFrameworks


triangleBlip : Float -> Float -> Svg msg
triangleBlip x y =
    path
        [ d "M412.201,311.406c0.021,0,0.042,0,0.063,0c0.067,0,0.135,0,0.201,0c4.052,0,6.106-0.051,8.168-0.102c2.053-0.051,4.115-0.102,8.176-0.102h0.103c6.976-0.183,10.227-5.306,6.306-11.53c-3.988-6.121-4.97-5.407-8.598-11.224c-1.631-3.008-3.872-4.577-6.179-4.577c-2.276,0-4.613,1.528-6.48,4.699c-3.578,6.077-3.26,6.014-7.306,11.723C402.598,306.067,405.426,311.406,412.201,311.406"
        , transform <| "scale(" ++ toString (22 / 34) ++ ") translate(" ++ toString (-404 + x * (34 / 22) - 17) ++ ", " ++ toString (-282 + y * (34 / 22) - 17) ++ ")"
        ]
        []


circleBlip : Float -> Float -> Svg msg
circleBlip x y =
    path
        [ d "M420.084,282.092c-1.073,0-2.16,0.103-3.243,0.313c-6.912,1.345-13.188,8.587-11.423,16.874c1.732,8.141,8.632,13.711,17.806,13.711c0.025,0,0.052,0,0.074-0.003c0.551-0.025,1.395-0.011,2.225-0.109c4.404-0.534,8.148-2.218,10.069-6.487c1.747-3.886,2.114-7.993,0.913-12.118C434.379,286.944,427.494,282.092,420.084,282.092"
        , transform <| "scale(" ++ toString (22 / 34) ++ ") translate(" ++ toString (-404 + x * (34 / 22) - 17) ++ ", " ++ toString (-282 + y * (34 / 22) - 17) ++ ")"
        ]
        []


type alias Position =
    { x : Float, y : Float }


randomBlipCoordinates : Float -> Float -> Float -> Generator Position
randomBlipCoordinates minRadius maxRadius startAngle =
    Random.andThen
        (\randRadius ->
            Random.map
                (toCartesian randRadius startAngle)
                (randomAngleFromRadius randRadius)
        )
        (Random.float (minRadius + 11) (maxRadius - 11))


randomAngleFromRadius : Float -> Generator Float
randomAngleFromRadius radius =
    let
        initialDelta =
            asin (11 / radius) * 180 / pi

        angleDelta =
            if initialDelta > 45 then
                45
            else
                initialDelta
    in
    Random.float angleDelta (90 - angleDelta)


toCartesian : Float -> Float -> Float -> Position
toCartesian radius angle startAngle =
    let
        adjustX =
            (sin << radians) startAngle - (cos << radians) startAngle

        adjustY =
            -1 * (cos << radians) startAngle - (sin << radians) startAngle
    in
    Position (radius * cos angle * adjustX) (radius * sin angle * adjustY)
