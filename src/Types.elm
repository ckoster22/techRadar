module Types exposing (AppState(..), Msg(..))

import Radar.Model exposing (Radar)


type Msg
    = ShowMockData


type AppState
    = ShowPrompt (Maybe String)
    | ShowRadar Radar
