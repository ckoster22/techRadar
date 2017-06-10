module Radar.View exposing (view)

import Html exposing (Html)
import Radar.Model exposing (Blip, Quadrant(..), Radar, Ring(..), determineCoordinatesForRadar, svgForBlip)
import Svg exposing (Attribute, Svg, g, path, svg)
import Svg.Attributes exposing (class, cx, cy, d, fill, height, r, width)
import Types exposing (Msg(..))


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


view : Radar -> Html Msg
view radar =
    let
        toolsBlips =
            List.filter (\blip -> blip.quadrant == Tools) radar

        techniquesBlips =
            List.filter (\blip -> blip.quadrant == Techniques) radar

        platformsBlips =
            List.filter (\blip -> blip.quadrant == Platforms) radar

        langsAndFrameworksBlips =
            List.filter (\blip -> blip.quadrant == LangsAndFrameworks) radar
    in
    svg
        [ width "800px", height "800px" ]
        [ g
            []
            [ quadrant Tools
            , quadrant Techniques
            , quadrant Platforms
            , quadrant LangsAndFrameworks
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


quadrant : Quadrant -> Svg Msg
quadrant quadrant =
    g
        [ class <| classForQuadrant quadrant ]
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
