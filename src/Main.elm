module Main exposing (main)

import Html exposing (Html)
import Landing.Util exposing (findSheetId, httpGetSheetById)
import Landing.View as LandingView
import Radar.Model exposing (Quadrant(..), Radar, Ring(..))
import Radar.View as RadarView
import Types exposing (AppState(..), Msg(..))


main : Program Never AppState Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( AppState, Cmd Msg )
init =
    ShowPrompt Nothing Nothing ! []


update : Msg -> AppState -> ( AppState, Cmd Msg )
update msg appState =
    case msg of
        RetrieveRadarData ->
            case appState of
                ShowPrompt (Just url) _ ->
                    let
                        cmd =
                            findSheetId url
                                |> Maybe.map httpGetSheetById
                                |> Maybe.withDefault Cmd.none
                    in
                    appState ! [ cmd ]

                _ ->
                    appState |> noCmd

        RetrieveRadarDataSuccess radar ->
            ShowRadar radar |> noCmd

        UpdateUrl url ->
            ShowPrompt (Just url) Nothing |> noCmd

        _ ->
            appState |> noCmd


noCmd : AppState -> ( AppState, Cmd Msg )
noCmd appState =
    appState ! []


view : AppState -> Html Msg
view appState =
    case appState of
        ShowPrompt url_ error_ ->
            LandingView.view url_ error_

        ShowRadar radar ->
            RadarView.view radar


mockRadar : Radar
mockRadar =
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
    ]
