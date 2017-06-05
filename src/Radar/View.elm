module Radar.View exposing (view)

import Html exposing (Html)
import Radar.Model exposing (Blip, Quadrant(..), Radar, Ring(..), circleBlip, determineCoordinatesForRing, triangleBlip)
import Svg exposing (Attribute, Svg, g, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, r, width)


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


ring1Color : String
ring1Color =
    "#BABABA"


ringPadding : Float
ringPadding =
    5


ring1Radius : Float
ring1Radius =
    150


ring2Color : String
ring2Color =
    "#CACACA"


ring2Radius : Float
ring2Radius =
    250


ring3Color : String
ring3Color =
    "#DADADA"


ring3Radius : Float
ring3Radius =
    325


ring4Color : String
ring4Color =
    "#EEEEEE"


ring4Radius : Float
ring4Radius =
    375


mockBlips : List Blip
mockBlips =
    [ { name = "tech1"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech2"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech3"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech4"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech5"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech6"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech7"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech8"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech2"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech3"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech4"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech5"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech6"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech7"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech8"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech2"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech3"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech4"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech5"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech6"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech7"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech8"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech2"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech3"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech4"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech5"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech6"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech7"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech8"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech2"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech3"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech4"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech5"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech6"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    , { name = "tech7"
      , ring = Hold
      , quadrant = Tools
      , isNew = False
      , description = "description"
      }
    ]


view : Radar -> Html msg
view radar =
    svg
        [ width "800px", height "800px" ]
        [ g
            []
            (fourRings ring1Radius ring1Color
                |> List.append (fourRings ring2Radius ring2Color)
                |> List.append (fourRings ring3Radius ring3Color)
                |> List.append (fourRings ring4Radius ring4Color)
            )
        , g
            []
            (List.map triangleBlip (determineCoordinatesForRing Hold mockBlips))
        ]


fourRings : Float -> String -> List (Svg msg)
fourRings ringRadius ringColor =
    [ ring UpperRight ringRadius ringColor
    , ring LowerRight ringRadius ringColor
    , ring LowerLeft ringRadius ringColor
    , ring UpperLeft ringRadius ringColor
    ]


ring : RingQuadrant -> Float -> String -> Svg msg
ring quadrant ringRadius ringColor =
    case quadrant of
        UpperRight ->
            path [ arc (radarCx + ringPadding) (radarCy - ringPadding) (ringRadius - ringPadding) 0 90, fill ringColor ] []

        LowerRight ->
            path [ arc (radarCx + ringPadding) (radarCy + ringPadding) (ringRadius - ringPadding) 90 180, fill ringColor ] []

        LowerLeft ->
            path [ arc (radarCx - ringPadding) (radarCy + ringPadding) (ringRadius - ringPadding) 180 270, fill ringColor ] []

        UpperLeft ->
            path [ arc (radarCx - ringPadding) (radarCy - ringPadding) (ringRadius - ringPadding) 270 360, fill ringColor ] []


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
