module Util.Svg exposing (arc)

import Svg exposing (Attribute)
import Svg.Attributes exposing (d)


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
    d <| "M" ++ String.fromFloat startX ++ " " ++ String.fromFloat startY ++ " A " ++ String.fromFloat radius ++ " " ++ String.fromFloat radius ++ " 0 " ++ largeArcFlag ++ " 0 " ++ String.fromFloat endX ++ " " ++ String.fromFloat endY ++ " L " ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ " Z"


polarToCartesian : Float -> Float -> Float -> Float -> ( Float, Float )
polarToCartesian cx cy radius angleDegrees =
    let
        angleInRadians =
            (angleDegrees - 90) * pi / 180
    in
    ( cx + radius * cos angleInRadians, cy + radius * sin angleInRadians )
