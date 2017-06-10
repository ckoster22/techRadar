module Radar.View exposing (view)

import Html exposing (Html)
import Radar.Model exposing (Blip, Quadrant(..), Radar, Ring(..), determineCoordinatesForRadar, svgForBlip)
import Svg exposing (Attribute, Svg, g, path, svg)
import Svg.Attributes exposing (class, cx, cy, d, fill, height, r, width)
import Types exposing (Msg(..))


type RingQuadrant
    = UpperRight
    | LowerRight
    | LowerLeft
    | UpperLeft


radarCx : Float
radarCx =
    400


radarCy : Float
radarCy =
    400



-- ring1Color : String
-- ring1Color =
--     "#BABABA"


ringPadding : Float
ringPadding =
    5


ring1Radius : Float
ring1Radius =
    150



-- ring2Color : String
-- ring2Color =
--     "#CACACA"


ring2Radius : Float
ring2Radius =
    250



-- ring3Color : String
-- ring3Color =
--     "#DADADA"


ring3Radius : Float
ring3Radius =
    325



-- ring4Color : String
-- ring4Color =
--     "#EEEEEE"


ring4Radius : Float
ring4Radius =
    375


view : Radar -> Html Msg
view radar =
    svg
        [ width "800px", height "800px" ]
        [ g
            []
            [ fourRings ring4Radius "radar-ring4"
            , fourRings ring3Radius "radar-ring3"
            , fourRings ring2Radius "radar-ring2"
            , fourRings ring1Radius "radar-ring1"
            ]
        , g
            []
            (List.map (svgForBlip True) (determineCoordinatesForRadar radar))
        ]


fourRings : Float -> String -> Svg Msg
fourRings ringRadius cssClass =
    g
        [ class cssClass ]
        [ ring UpperRight ringRadius
        , ring LowerRight ringRadius
        , ring LowerLeft ringRadius
        , ring UpperLeft ringRadius
        ]


ring : RingQuadrant -> Float -> Svg Msg
ring quadrant ringRadius =
    case quadrant of
        UpperRight ->
            path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ringRadius - ringPadding) 0 90 ] []

        LowerRight ->
            path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ringRadius - ringPadding) 90 180 ] []

        LowerLeft ->
            path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ringRadius - ringPadding) 180 270 ] []

        UpperLeft ->
            path [ arc (radarCx - ringPadding) (radarCy - ringPadding) (ringRadius - ringPadding) 270 360 ] []


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
