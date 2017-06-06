module Landing.View exposing (view)

import Html exposing (Html, button, text)
import Html.Events exposing (onClick)
import Types exposing (Msg(..))


view : Maybe String -> Html Msg
view url_ =
    button
        [ onClick ShowMockData ]
        [ text "Show mock radar" ]
