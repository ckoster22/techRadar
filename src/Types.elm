module Types exposing (AppState(..), Msg(..))

import Radar.Model exposing (Radar)


type Msg
    = RetrieveRadarData
    | RetrieveRadarDataSuccess Radar
    | RetrieveRadarDataFailure String
    | UpdateUrl String


type AppState
    = ShowPrompt (Maybe String) (Maybe String)
    | ShowRadar Radar
