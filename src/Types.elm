module Types exposing (AppState(..), Msg(..))

import Radar.Model exposing (Radar)


type Msg
    = ShowMockData
    | UpdateUrl String


type AppState
    = ShowPrompt (Maybe String)
    | ShowRadar Radar
