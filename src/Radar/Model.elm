module Radar.Model exposing (Blip, Quadrant(..), Radar, Ring(..), csvToMaybeBlip, determineCoordinatesForRadar, svgForBlip)

import Random exposing (Generator)
import Random.Extra as RandomExtra
import Svg exposing (Svg, path, text)
import Svg.Attributes exposing (d, transform)


blipWidth : Float
blipWidth =
    22


type alias Radar =
    List Blip


type alias Blip =
    { name : String
    , rowNum : Int
    , ring : Ring
    , quadrant : Quadrant
    , isNew : Bool
    , description : String
    , position_ : Maybe Position
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


type alias Position =
    { x : Float, y : Float }


getRing : String -> Result String Ring
getRing ringStr =
    if ringStr == "hold" then
        Ok Hold
    else if ringStr == "assess" then
        Ok Assess
    else if ringStr == "trial" then
        Ok Trial
    else if ringStr == "adopt" then
        Ok Adopt
    else
        Err <| "Invalid ring value" ++ ringStr


getQuadrant : String -> Result String Quadrant
getQuadrant quadrantStr =
    if quadrantStr == "tools" then
        Ok Tools
    else if quadrantStr == "techniques" then
        Ok Techniques
    else if quadrantStr == "platforms" then
        Ok Platforms
    else if quadrantStr == "languages & frameworks" then
        Ok LangsAndFrameworks
    else
        Err <| "Invalid quadrant value" ++ quadrantStr


getNew : String -> Result String Bool
getNew isNewStr =
    if isNewStr == "TRUE" then
        Ok True
    else if isNewStr == "FALSE" then
        Ok False
    else
        Err <| "Invalid isNew value" ++ isNewStr


csvToMaybeBlip : String -> Int -> Maybe Blip
csvToMaybeBlip csv index =
    case String.split "," csv of
        name :: ringStr :: quadrantStr :: isNewStr :: description :: _ ->
            case ( getRing ringStr, getQuadrant quadrantStr, getNew isNewStr ) of
                ( Ok ring, Ok quadrant, Ok isNew ) ->
                    Just <| Blip name index ring quadrant isNew description Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


radiusesForRing : Ring -> ( Float, Float )
radiusesForRing ring =
    case ring of
        Hold ->
            ( 325, 375 )

        Assess ->
            ( 250, 325 )

        Trial ->
            ( 150, 250 )

        Adopt ->
            ( 0, 150 )



-- svgForBlips : List Blip -> List (Svg msg)
-- svgForBlips blips =
--     List.map


svgForBlip : Blip -> Svg msg
svgForBlip blip =
    case blip.position_ of
        Just position ->
            if blip.isNew then
                triangleBlip position
            else
                circleBlip position

        Nothing ->
            -- TODO: this shouldn't be possible.. fix it
            text ""


triangleBlip : Position -> Svg msg
triangleBlip pos =
    path
        [ d "M412.201,311.406c0.021,0,0.042,0,0.063,0c0.067,0,0.135,0,0.201,0c4.052,0,6.106-0.051,8.168-0.102c2.053-0.051,4.115-0.102,8.176-0.102h0.103c6.976-0.183,10.227-5.306,6.306-11.53c-3.988-6.121-4.97-5.407-8.598-11.224c-1.631-3.008-3.872-4.577-6.179-4.577c-2.276,0-4.613,1.528-6.48,4.699c-3.578,6.077-3.26,6.014-7.306,11.723C402.598,306.067,405.426,311.406,412.201,311.406"
        , transform <| "scale(" ++ toString (blipWidth / 34) ++ ") translate(" ++ toString (-404 + pos.x * (34 / blipWidth) - 17) ++ ", " ++ toString (-282 + pos.y * (34 / blipWidth) - 17) ++ ")"
        ]
        []


circleBlip : Position -> Svg msg
circleBlip pos =
    path
        [ d "M420.084,282.092c-1.073,0-2.16,0.103-3.243,0.313c-6.912,1.345-13.188,8.587-11.423,16.874c1.732,8.141,8.632,13.711,17.806,13.711c0.025,0,0.052,0,0.074-0.003c0.551-0.025,1.395-0.011,2.225-0.109c4.404-0.534,8.148-2.218,10.069-6.487c1.747-3.886,2.114-7.993,0.913-12.118C434.379,286.944,427.494,282.092,420.084,282.092"
        , transform <| "scale(" ++ toString (blipWidth / 34) ++ ") translate(" ++ toString (-404 + pos.x * (34 / blipWidth) - 17) ++ ", " ++ toString (-282 + pos.y * (34 / blipWidth) - 17) ++ ")"
        ]
        []


determineCoordinatesForRadar : List Blip -> List Blip
determineCoordinatesForRadar blips =
    List.foldl
        (\blip ( accBlips, seed ) ->
            let
                generator =
                    accBlips
                        |> List.concatMap
                            (\blip ->
                                blip.position_
                                    |> Maybe.map (\position -> [ position ])
                                    |> Maybe.withDefault []
                            )
                        |> findCoordForBlip 0 blip

                ( randBlip, nextSeed ) =
                    Random.step generator seed
            in
            ( randBlip :: accBlips, nextSeed )
        )
        ( [], Random.initialSeed 12345 )
        blips
        |> Tuple.first


startAngleForQuadrant : Quadrant -> Float
startAngleForQuadrant quadrant =
    case quadrant of
        Tools ->
            0

        Techniques ->
            pi / 2

        Platforms ->
            -pi / 2

        LangsAndFrameworks ->
            -pi


findCoordForBlip : Int -> Blip -> List Position -> Generator Blip
findCoordForBlip iteration blip positions =
    Random.andThen
        (\randPosition ->
            if doesCoordinateCollide randPosition positions && iteration < 100 then
                findCoordForBlip (iteration + 1) blip positions
            else
                { blip | position_ = Just randPosition } |> RandomExtra.constant
        )
        (randomBlipCoordinates blip.ring (startAngleForQuadrant blip.quadrant))


doesCoordinateCollide : Position -> List Position -> Bool
doesCoordinateCollide position positions =
    List.any
        (\currPosition ->
            (abs (currPosition.x - position.x) < blipWidth) && (abs currPosition.y - position.y < blipWidth)
        )
        positions


randomBlipCoordinates : Ring -> Float -> Generator Position
randomBlipCoordinates ring startAngle =
    let
        ( minRadius, maxRadius ) =
            radiusesForRing ring
    in
    Random.andThen
        (\randRadius ->
            Random.map
                (toCartesian randRadius startAngle)
                (randomAngleFromRadius randRadius)
        )
        (Random.float (minRadius + blipWidth / 2) (maxRadius - blipWidth / 2))


randomAngleFromRadius : Float -> Generator Float
randomAngleFromRadius radius =
    let
        initialDelta =
            asin (blipWidth / 2 / radius) * 180 / pi

        angleDelta =
            if initialDelta > 45 then
                45
            else
                initialDelta
    in
    Random.map
        (\delta ->
            pi * delta / 180
        )
        (Random.float angleDelta (90 - angleDelta))


toCartesian : Float -> Float -> Float -> Position
toCartesian radius startAngle angle =
    let
        adjustX =
            sin startAngle - cos startAngle

        adjustY =
            -1 * cos startAngle - sin startAngle
    in
    Position (400 + radius * cos angle * adjustX) (400 + radius * sin angle * adjustY)
