module Main exposing (main)

import Html exposing (Html, button, text)
import Html.Events exposing (onClick)
import Radar.Model exposing (Quadrant(..), Radar, Ring(..))
import Radar.View as RadarView


main : Program Never AppState Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type Msg
    = ShowMockData


type AppState
    = Init
    | ShowRadar Radar


init : ( AppState, Cmd Msg )
init =
    Init ! []


update : Msg -> AppState -> ( AppState, Cmd Msg )
update msg appState =
    case msg of
        ShowMockData ->
            ShowRadar mockRadar ! []


view : AppState -> Html Msg
view appState =
    case appState of
        Init ->
            button
                [ onClick ShowMockData ]
                [ text "Show mock radar" ]

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
