module Main exposing (main)

import Html
import Radar.Model as RadarModel
import Radar.View as RadarView


main : Program Never RadarModel.Radar Msg
main =
    Html.program
        { init = [] ! []
        , update = \_ model -> model ! []
        , subscriptions = \_ -> Sub.none
        , view = RadarView.view
        }


type Msg
    = Msg
