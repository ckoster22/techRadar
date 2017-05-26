module Radar.View exposing (view)

import Html exposing (Html)
import Radar.Model exposing (Radar)
import Svg exposing (Attribute, Svg, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, r, width)


view : Radar -> Html msg
view radar =
    svg
        [ width "600px", height "600px" ]
        [ path [ arc 150 150 150 90 180, fill "#FF0000" ] [] ]


arc : Float -> Float -> Float -> Float -> Float -> Attribute msg
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
