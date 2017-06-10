module Types exposing (AppState(..), Msg(..))

import Radar.Model exposing (Quadrant, Radar)


type Msg
    = RetrieveRadarData
    | RetrieveRadarDataSuccess Radar
    | RetrieveRadarDataFailure String
    | UpdateUrl String
    | MouseoverQuadrant Quadrant
    | MouseoutQuadrant


type AppState
    = ShowPrompt (Maybe String) (Maybe String)
    | ShowRadar Radar (Maybe Quadrant)
