module Main exposing (main)

import Data.Radar exposing (Blip)
import Html exposing (Html, div)
import Html.Attributes
import Page.Landing as LandingPage exposing (Msg(..))
import Page.Radar as RadarPage


type Page
    = LandingPage LandingPage.Model
    | RadarPage RadarPage.Model


type alias AppState =
    { page : Page
    , data : Maybe (List Blip)
    }


type Msg
    = LandingPageMsg LandingPage.Msg
    | RadarPageMsg RadarPage.Msg


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
    ( { page = LandingPage LandingPage.initialModel
      , data = Nothing
      }
    , Cmd.none
    )


update : Msg -> AppState -> ( AppState, Cmd Msg )
update msg appState =
    updatePage appState.page msg appState


updatePage : Page -> Msg -> AppState -> ( AppState, Cmd Msg )
updatePage page msg appState =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { appState | page = toModel newModel }, Cmd.map toMsg newCmd )
    in
    case ( Debug.log "msg" msg, page ) of
        ( LandingPageMsg (RetrieveRadarDataSuccess blips), LandingPage landingPageModel ) ->
            ( { appState | page = RadarPage (RadarPage.Model blips Nothing) }, Cmd.none )

        ( LandingPageMsg landingPageMsg, LandingPage landingPageModel ) ->
            toPage LandingPage LandingPageMsg LandingPage.update landingPageMsg landingPageModel

        ( RadarPageMsg radarPageMsg, RadarPage radarPageModel ) ->
            toPage RadarPage RadarPageMsg RadarPage.update radarPageMsg radarPageModel

        ( _, _ ) ->
            appState ! []


view : AppState -> Html Msg
view appState =
    let
        appView =
            viewPage False appState.page
    in
    -- https://github.com/elm-lang/elm-reactor/issues/138
    elmReactorCssWorkaround appView


viewPage : Bool -> Page -> Html Msg
viewPage isLoading page =
    case page of
        LandingPage model ->
            LandingPage.view model
                |> Html.map LandingPageMsg

        RadarPage model ->
            RadarPage.view model
                |> Html.map RadarPageMsg


elmReactorCssWorkaround : Html Msg -> Html Msg
elmReactorCssWorkaround appView =
    div
        []
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , appView
        ]
