module Page.Radar exposing (Model, Msg, initialModel, update, view)

import Data.Radar exposing (GoogleSheetBlip, Quadrant(..), Ring(..))
import Html exposing (Html, div, h3, li, p, ul)
import Random exposing (Generator)
import Svg exposing (Attribute, Svg, g, path, svg, text, text_)
import Svg.Attributes exposing (class, cx, cy, d, fill, height, r, transform, width, x, y)
import Svg.Events exposing (onClick, onMouseOut, onMouseOver)
import Util.Svg as SvgUtil


type alias Model =
    { blips : List GoogleSheetBlip
    , selectionState : SelectionState
    , errors_ : Maybe (List String)
    }


initialModel : List GoogleSheetBlip -> Maybe (List String) -> Model
initialModel blips errors_ =
    Model blips NoHighlightOrSelection errors_


type SelectionState
    = NoHighlightOrSelection
    | Highlight Quadrant
    | Select Quadrant


type alias PositionedBlip =
    { sheetBlip : GoogleSheetBlip
    , position : Position
    }


type alias Position =
    { x : Float, y : Float }


type Msg
    = MouseoverQuadrant Quadrant
    | MouseoutQuadrant
    | OnQuadrantClick Quadrant


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseoverQuadrant quadrant ->
            case model.selectionState of
                NoHighlightOrSelection ->
                    ( { model | selectionState = Highlight quadrant }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        MouseoutQuadrant ->
            case model.selectionState of
                Highlight _ ->
                    ( { model | selectionState = NoHighlightOrSelection }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        OnQuadrantClick quadrant ->
            ( { model | selectionState = Select quadrant }
            , Cmd.none
            )



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

        highlightedQuadrant_ =
            case model.selectionState of
                Highlight quadrant ->
                    Just quadrant

                Select quadrant ->
                    Just quadrant

                _ ->
                    Nothing

        selectedQuadrant_ =
            case model.selectionState of
                Select quadrant ->
                    Just quadrant

                _ ->
                    Nothing
    in
    div
        [ class "radar-container" ]
        [ div []
            [ errorSection model.errors_
            , svg
                [ width "800px", height "800px" ]
                [ g
                    []
                    [ makeQuadrant Tools highlightedQuadrant_
                    , makeQuadrant LangsAndFrameworks highlightedQuadrant_
                    , makeQuadrant Platforms highlightedQuadrant_
                    , makeQuadrant Techniques highlightedQuadrant_
                    ]
                , g
                    []
                    [ blipsGrouping toolsBlips Tools
                    , blipsGrouping techniquesBlips Techniques
                    , blipsGrouping platformsBlips Platforms
                    , blipsGrouping langsAndFrameworksBlips LangsAndFrameworks
                    ]
                ]
            ]
        , detailsSection selectedQuadrant_ model.blips
        ]


errorSection : Maybe (List String) -> Html Msg
errorSection errors_ =
    case errors_ of
        Just errors ->
            div
                []
                (List.map (\error -> p [] [ text error ]) errors)

        Nothing ->
            text ""


detailsSection : Maybe Quadrant -> List GoogleSheetBlip -> Html Msg
detailsSection selectedQuadrant_ blips =
    case selectedQuadrant_ of
        Just quadrant ->
            let
                ( ( adoptBlips, trialBlips ), ( assessBlips, holdBlips ) ) =
                    blips
                        |> List.filter (\blip -> blip.quadrant == quadrant)
                        |> List.foldl
                            (\blip ( ( adopt, trial ), ( assess, hold ) ) ->
                                case blip.ring of
                                    Adopt ->
                                        ( ( blip :: adopt, trial ), ( assess, hold ) )

                                    Trial ->
                                        ( ( adopt, blip :: trial ), ( assess, hold ) )

                                    Assess ->
                                        ( ( adopt, trial ), ( blip :: assess, hold ) )

                                    Hold ->
                                        ( ( adopt, trial ), ( assess, blip :: hold ) )
                            )
                            ( ( [], [] ), ( [], [] ) )
            in
            div
                []
                [ detailsForRing "Adopt" adoptBlips
                , detailsForRing "Trial" trialBlips
                , detailsForRing "Assess" assessBlips
                , detailsForRing "Hold" holdBlips
                ]

        Nothing ->
            text ""


detailsForRing : String -> List GoogleSheetBlip -> Html Msg
detailsForRing ringName blips =
    div
        []
        [ h3 [] [ text ringName ]
        , ul
            []
            (List.map (\blip -> li [] [ text blip.name ]) blips)
        ]


blipsGrouping : List GoogleSheetBlip -> Quadrant -> Svg Msg
blipsGrouping blips quadrant =
    g
        [ class <| "quad " ++ classForQuadrant quadrant ]
        (blips
            |> determineCoordinatesForRadar
            |> List.map
                (\blip ->
                    svgForBlip blip.position blip.sheetBlip.rowNum blip.sheetBlip.isNew
                )
        )


makeQuadrant : Quadrant -> Maybe Quadrant -> Svg Msg
makeQuadrant quadrant highlightQuadrant_ =
    g
        [ class <| "quad " ++ classForQuadrant quadrant ++ classForHighlight quadrant highlightQuadrant_
        , onMouseOver <| MouseoverQuadrant quadrant
        , onMouseOut <| MouseoutQuadrant
        , onClick <| OnQuadrantClick quadrant
        ]
        (ringsForQuadrant quadrant)


classForQuadrant : Quadrant -> String
classForQuadrant quadrant =
    case quadrant of
        Tools ->
            "quad-tools"

        Techniques ->
            "quad-techniques"

        Platforms ->
            "quad-platforms"

        LangsAndFrameworks ->
            "quad-langsAndFrameworks"


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
            fourRings (radarCx + ringPadding) (radarCy - ringPadding) 0

        Techniques ->
            fourRings (radarCx - ringPadding) (radarCy - ringPadding) 270

        Platforms ->
            fourRings (radarCx - ringPadding) (radarCy + ringPadding) 180

        LangsAndFrameworks ->
            fourRings (radarCx + ringPadding) (radarCy + ringPadding) 90


fourRings : Float -> Float -> Float -> List (Svg Msg)
fourRings x y startAngle =
    [ g [ class "radar-ring4" ] [ path [ SvgUtil.arc x y (ring4Radius - ringPadding) startAngle (startAngle + 90) ] [] ]
    , g [ class "radar-ring3" ] [ path [ SvgUtil.arc x y (ring3Radius - ringPadding) startAngle (startAngle + 90) ] [] ]
    , g [ class "radar-ring2" ] [ path [ SvgUtil.arc x y (ring2Radius - ringPadding) startAngle (startAngle + 90) ] [] ]
    , g [ class "radar-ring1" ] [ path [ SvgUtil.arc x y (ring1Radius - ringPadding) startAngle (startAngle + 90) ] [] ]
    ]


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
            , x <| String.fromFloat (position.x - 4)
            , y <| String.fromFloat (position.y + 2)
            ]
            [ text <| String.fromInt rowNum ]
        ]


triangleBlip : Position -> Svg msg
triangleBlip pos =
    path
        [ d "M412.201,311.406c0.021,0,0.042,0,0.063,0c0.067,0,0.135,0,0.201,0c4.052,0,6.106-0.051,8.168-0.102c2.053-0.051,4.115-0.102,8.176-0.102h0.103c6.976-0.183,10.227-5.306,6.306-11.53c-3.988-6.121-4.97-5.407-8.598-11.224c-1.631-3.008-3.872-4.577-6.179-4.577c-2.276,0-4.613,1.528-6.48,4.699c-3.578,6.077-3.26,6.014-7.306,11.723C402.598,306.067,405.426,311.406,412.201,311.406"
        , transform <| "scale(" ++ String.fromFloat (blipWidth / 34) ++ ") translate(" ++ String.fromFloat (-404 + pos.x * (34 / blipWidth) - 17) ++ ", " ++ String.fromFloat (-282 + pos.y * (34 / blipWidth) - 17) ++ ")"
        ]
        []


circleBlip : Position -> Svg msg
circleBlip pos =
    path
        [ d "M420.084,282.092c-1.073,0-2.16,0.103-3.243,0.313c-6.912,1.345-13.188,8.587-11.423,16.874c1.732,8.141,8.632,13.711,17.806,13.711c0.025,0,0.052,0,0.074-0.003c0.551-0.025,1.395-0.011,2.225-0.109c4.404-0.534,8.148-2.218,10.069-6.487c1.747-3.886,2.114-7.993,0.913-12.118C434.379,286.944,427.494,282.092,420.084,282.092"
        , transform <| "scale(" ++ String.fromFloat (blipWidth / 34) ++ ") translate(" ++ String.fromFloat (-404 + pos.x * (34 / blipWidth) - 17) ++ ", " ++ String.fromFloat (-282 + pos.y * (34 / blipWidth) - 17) ++ ")"
        ]
        []


determineCoordinatesForRadar : List GoogleSheetBlip -> List PositionedBlip
determineCoordinatesForRadar blips =
    List.foldl
        (\blip ( accBlips, seed ) ->
            let
                generator =
                    accBlips
                        |> List.map .position
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
            pi / 2

        Techniques ->
            0

        Platforms ->
            -pi / 2

        LangsAndFrameworks ->
            -pi


findCoordForBlip : Int -> GoogleSheetBlip -> List Position -> Generator PositionedBlip
findCoordForBlip iteration blip positions =
    Random.andThen
        (\randPosition ->
            if doesCoordinateCollide randPosition positions && iteration < 100 then
                findCoordForBlip (iteration + 1) blip positions

            else
                PositionedBlip blip randPosition |> Random.constant
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
