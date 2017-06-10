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
            [ quadrant UpperRight
            , quadrant LowerRight
            , quadrant LowerLeft
            , quadrant UpperLeft
            ]
        , g
            []
            (List.map (svgForBlip True) (determineCoordinatesForRadar radar))
        ]


quadrant : RingQuadrant -> Svg Msg
quadrant quadrant =
    let
        quadrantClass =
            case quadrant of
                UpperRight ->
                    "quad-tools"

                LowerRight ->
                    "quad-langsAndFrameworks"

                LowerLeft ->
                    "quad-platforms"

                UpperLeft ->
                    "quad-techniques"
    in
    g
        [ class quadrantClass ]
        (ringsForQuadrant quadrant)


ringsForQuadrant : RingQuadrant -> List (Svg Msg)
ringsForQuadrant quadrant =
    case quadrant of
        UpperRight ->
            [ g [ class "radar-ring4" ] [ path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ring4Radius - ringPadding) 0 90 ] [] ]
            , g [ class "radar-ring3" ] [ path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ring3Radius - ringPadding) 0 90 ] [] ]
            , g [ class "radar-ring2" ] [ path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ring2Radius - ringPadding) 0 90 ] [] ]
            , g [ class "radar-ring1" ] [ path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ring1Radius - ringPadding) 0 90 ] [] ]
            ]

        LowerRight ->
            [ g [ class "radar-ring4" ] [ path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ring4Radius - ringPadding) 90 180 ] [] ]
            , g [ class "radar-ring3" ] [ path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ring3Radius - ringPadding) 90 180 ] [] ]
            , g [ class "radar-ring2" ] [ path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ring2Radius - ringPadding) 90 180 ] [] ]
            , g [ class "radar-ring1" ] [ path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ring1Radius - ringPadding) 90 180 ] [] ]
            ]

        LowerLeft ->
            [ g [ class "radar-ring4" ] [ path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ring4Radius - ringPadding) 180 270 ] [] ]
            , g [ class "radar-ring3" ] [ path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ring3Radius - ringPadding) 180 270 ] [] ]
            , g [ class "radar-ring2" ] [ path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ring2Radius - ringPadding) 180 270 ] [] ]
            , g [ class "radar-ring1" ] [ path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ring1Radius - ringPadding) 180 270 ] [] ]
            ]

        UpperLeft ->
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
