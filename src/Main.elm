module Main exposing (main)

import Html exposing (Html, div)
import Html.Attributes
import Page.Landing as LandingPage exposing (Msg(..))
import Page.Radar as RadarPage


type Page
    = LandingPage LandingPage.Model
    | RadarPage RadarPage.Model


type Msg
    = LandingPageMsg LandingPage.Msg
    | RadarPageMsg RadarPage.Msg


main : Program Never Page Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( Page, Cmd Msg )
init =
    ( LandingPage LandingPage.initialModel, Cmd.none )


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( toModel newModel, Cmd.map toMsg newCmd )
    in
    case ( msg, page ) of
        ( LandingPageMsg (RetrieveRadarDataSuccess blips errors_), LandingPage landingPageModel ) ->
            ( RadarPage (RadarPage.Model blips Nothing errors_), Cmd.none )

        ( LandingPageMsg landingPageMsg, LandingPage landingPageModel ) ->
            toPage LandingPage LandingPageMsg LandingPage.update landingPageMsg landingPageModel

        ( RadarPageMsg radarPageMsg, RadarPage radarPageModel ) ->
            toPage RadarPage RadarPageMsg RadarPage.update radarPageMsg radarPageModel

        ( _, _ ) ->
            page ! []


view : Page -> Html Msg
view page =
    let
        appView =
            viewPage False page
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
