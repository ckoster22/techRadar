module Main exposing (main)

import Html exposing (Html, div)
import Html.Attributes
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
    ShowPrompt (Just "https://docs.google.com/spreadsheets/d/11Fd0lwNIEUs2ymxNEiTfpM5CoQHQbMuOId8TjrQHDn0/edit#gid=0") Nothing ! []


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
    let
        appView =
            case appState of
                ShowPrompt url_ error_ ->
                    LandingView.view url_ error_

                ShowRadar radar ->
                    RadarView.view radar
    in
    -- https://github.com/elm-lang/elm-reactor/issues/138
    elmReactorCssWorkaround appView


elmReactorCssWorkaround : Html Msg -> Html Msg
elmReactorCssWorkaround appView =
    div
        []
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , appView
        ]
