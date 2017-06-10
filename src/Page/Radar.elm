module Page.Radar exposing (Model, Msg, update, view)

import Data.Radar exposing (Blip, Position, Quadrant(..), Ring(..))
import Html exposing (Html)
import Random exposing (Generator)
import Random.Extra as RandomExtra
import Svg exposing (Attribute, Svg, g, path, svg, text, text_)
import Svg.Attributes exposing (class, cx, cy, d, fill, height, r, transform, width, x, y)
import Svg.Events exposing (onMouseOut, onMouseOver)


type alias Model =
    { blips : List Blip
    , highlightedQuadrant_ : Maybe Quadrant
    }


type Msg
    = MouseoverQuadrant Quadrant
    | MouseoutQuadrant


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseoverQuadrant quadrant ->
            { model | highlightedQuadrant_ = Just quadrant } ! []

        MouseoutQuadrant ->
            { model | highlightedQuadrant_ = Nothing } ! []



-- View


blipWidth : Float
blipWidth =
    22


radarCx : Float
radarCx =
    400


radarCy : Float
radarCy =
    400


ringPadding : Float
ringPadding =
    5


ring1Radius : Float
ring1Radius =
    150


ring2Radius : Float
ring2Radius =
    250


ring3Radius : Float
ring3Radius =
    325


ring4Radius : Float
ring4Radius =
    375


view : Model -> Html Msg
view model =
    let
        toolsBlips =
            List.filter (\blip -> blip.quadrant == Tools) model.blips

        techniquesBlips =
            List.filter (\blip -> blip.quadrant == Techniques) model.blips

        platformsBlips =
            List.filter (\blip -> blip.quadrant == Platforms) model.blips

        langsAndFrameworksBlips =
            List.filter (\blip -> blip.quadrant == LangsAndFrameworks) model.blips
    in
    svg
        [ width "800px", height "800px" ]
        [ g
            []
            [ quadrant Tools model.highlightedQuadrant_
            , quadrant Techniques model.highlightedQuadrant_
            , quadrant Platforms model.highlightedQuadrant_
            , quadrant LangsAndFrameworks model.highlightedQuadrant_
            ]
        , g
            []
            [ blipsGrouping toolsBlips Tools
            , blipsGrouping techniquesBlips Techniques
            , blipsGrouping platformsBlips Platforms
            , blipsGrouping langsAndFrameworksBlips LangsAndFrameworks
            ]
        ]


blipsGrouping : List Blip -> Quadrant -> Svg Msg
blipsGrouping blips quadrant =
    g
        [ class <| classForQuadrant quadrant ]
        (blips
            |> determineCoordinatesForRadar
            |> List.map
                (\blip ->
                    case blip.position_ of
                        Just position ->
                            svgForBlip position blip.rowNum blip.isNew

                        Nothing ->
                            Svg.text ""
                )
        )


quadrant : Quadrant -> Maybe Quadrant -> Svg Msg
quadrant quadrant highlightQuadrant_ =
    g
        [ class <| "quad " ++ classForQuadrant quadrant ++ classForHighlight quadrant highlightQuadrant_
        , onMouseOver <| MouseoverQuadrant quadrant
        , onMouseOut <| MouseoutQuadrant
        ]
        (ringsForQuadrant quadrant)


classForQuadrant : Quadrant -> String
classForQuadrant quadrant =
    case quadrant of
        Tools ->
            "quad-tools"

        Techniques ->
            "quad-langsAndFrameworks"

        Platforms ->
            "quad-platforms"

        LangsAndFrameworks ->
            "quad-techniques"


classForHighlight : Quadrant -> Maybe Quadrant -> String
classForHighlight quadrant highlightQuadrant_ =
    case highlightQuadrant_ of
        Just highlightQuadrant ->
            if highlightQuadrant == quadrant then
                ""
            else
                " is-faded"

        Nothing ->
            ""


ringsForQuadrant : Quadrant -> List (Svg Msg)
ringsForQuadrant quadrant =
    case quadrant of
        Tools ->
            [ g [ class "radar-ring4" ] [ path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ring4Radius - ringPadding) 0 90 ] [] ]
            , g [ class "radar-ring3" ] [ path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ring3Radius - ringPadding) 0 90 ] [] ]
            , g [ class "radar-ring2" ] [ path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ring2Radius - ringPadding) 0 90 ] [] ]
            , g [ class "radar-ring1" ] [ path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ring1Radius - ringPadding) 0 90 ] [] ]
            ]

        Techniques ->
            [ g [ class "radar-ring4" ] [ path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ring4Radius - ringPadding) 90 180 ] [] ]
            , g [ class "radar-ring3" ] [ path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ring3Radius - ringPadding) 90 180 ] [] ]
            , g [ class "radar-ring2" ] [ path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ring2Radius - ringPadding) 90 180 ] [] ]
            , g [ class "radar-ring1" ] [ path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ring1Radius - ringPadding) 90 180 ] [] ]
            ]

        Platforms ->
            [ g [ class "radar-ring4" ] [ path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ring4Radius - ringPadding) 180 270 ] [] ]
            , g [ class "radar-ring3" ] [ path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ring3Radius - ringPadding) 180 270 ] [] ]
            , g [ class "radar-ring2" ] [ path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ring2Radius - ringPadding) 180 270 ] [] ]
            , g [ class "radar-ring1" ] [ path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ring1Radius - ringPadding) 180 270 ] [] ]
            ]

        LangsAndFrameworks ->
            [ g [ class "radar-ring4" ] [ path [ arc (radarCx - ringPadding) (radarCy - ringPadding) (ring4Radius - ringPadding) 270 360 ] [] ]
            , g [ class "radar-ring3" ] [ path [ arc (radarCx - ringPadding) (radarCy - ringPadding) (ring3Radius - ringPadding) 270 360 ] [] ]
            , g [ class "radar-ring2" ] [ path [ arc (radarCx - ringPadding) (radarCy - ringPadding) (ring2Radius - ringPadding) 270 360 ] [] ]
            , g [ class "radar-ring1" ] [ path [ arc (radarCx - ringPadding) (radarCy - ringPadding) (ring1Radius - ringPadding) 270 360 ] [] ]
            ]


arc : Float -> Float -> Float -> Float -> Float -> Attribute Msg
arc x y radius startAngle endAngle =
    let
        ( startX, startY ) =
            polarToCartesian x y radius endAngle

        ( endX, endY ) =
            polarToCartesian x y radius startAngle

        largeArcFlag =
            if (endAngle - startAngle) <= 180 then
                "0"
            else
                "1"
    in
    d <| "M" ++ toString startX ++ " " ++ toString startY ++ " A " ++ toString radius ++ " " ++ toString radius ++ " 0 " ++ largeArcFlag ++ " 0 " ++ toString endX ++ " " ++ toString endY ++ " L " ++ toString x ++ " " ++ toString y ++ " Z"


polarToCartesian : Float -> Float -> Float -> Float -> ( Float, Float )
polarToCartesian cx cy radius angleDegrees =
    let
        angleInRadians =
            (angleDegrees - 90) * pi / 180
    in
    ( cx + radius * cos angleInRadians, cy + radius * sin angleInRadians )


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


svgForBlip : Position -> Int -> Bool -> Svg msg
svgForBlip position rowNum isNew =
    let
        blipPathSvg =
            if isNew then
                triangleBlip position
            else
                circleBlip position
    in
    g
        []
        [ blipPathSvg
        , text_
            [ class "blip"
            , x <| toString (position.x - 4)
            , y <| toString (position.y + 2)
            ]
            [ text <| toString rowNum ]
        ]


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
