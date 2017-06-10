module Page.Landing exposing (Model, Msg(..), initialModel, update, view)

import Data.Radar exposing (Blip, Quadrant(..), Ring(..))
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Maybe.Extra as MaybeExtra
import Regex exposing (HowMany(..), Match, Regex, find, regex)


type alias Model =
    { url : String
    , error_ : Maybe String
    }


initialModel : Model
initialModel =
    Model "" Nothing


type Msg
    = RetrieveRadarData
    | RetrieveRadarDataSuccess (List Blip)
    | RetrieveRadarDataFailure String
    | UpdateUrl String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RetrieveRadarData ->
            case findSheetId model.url of
                Ok url ->
                    model ! [ httpGetSheetById url ]

                Err error ->
                    { model | error_ = Just error } ! []

        RetrieveRadarDataSuccess radar ->
            -- This is handled in Main.elm
            model ! []

        RetrieveRadarDataFailure error ->
            { model | error_ = Debug.log "error" <| Just error } ! []

        UpdateUrl url ->
            { model | url = url } ! []


sheetIdRegex : Regex
sheetIdRegex =
    regex "https:\\/\\/docs.google.com\\/spreadsheets\\/d\\/(.*?)($|\\/$|\\/.*|\\?.*)"


findSheetId : String -> Result String String
findSheetId url =
    find All sheetIdRegex url
        |> List.head
        |> Maybe.map .submatches
        |> Maybe.map (List.head >> MaybeExtra.join)
        |> MaybeExtra.join
        |> Maybe.map Ok
        |> Maybe.withDefault (Err "Unable to parse Google Sheet ID")


sheetJsonUrl : String -> String
sheetJsonUrl sheetId =
    "http://docs.google.com/spreadsheets/d/" ++ sheetId ++ "/export?gid=0&format=csv"


httpResultToMsg : Result Http.Error String -> Msg
httpResultToMsg result =
    case Debug.log "result" result of
        Ok csv ->
            let
                sheetRows =
                    case String.split "\x0D\n" csv of
                        title :: rest ->
                            rest

                        _ ->
                            []

                blips =
                    List.foldl
                        (\row ( blips, index ) ->
                            case csvToMaybeBlip row index of
                                Just blip ->
                                    ( blip :: blips, index + 1 )

                                Nothing ->
                                    ( blips, index + 1 )
                        )
                        ( [], 0 )
                        sheetRows
                        |> Tuple.first
            in
            RetrieveRadarDataSuccess blips

        Err httpError ->
            RetrieveRadarDataFailure "Unable to retrieve Google Sheet"


httpGetSheetById : String -> Cmd Msg
httpGetSheetById sheetId =
    sheetJsonUrl sheetId
        |> Http.getString
        |> Http.send httpResultToMsg


csvToMaybeBlip : String -> Int -> Maybe Blip
csvToMaybeBlip csv rowNum =
    case String.split "," csv of
        name :: ringStr :: quadrantStr :: isNewStr :: description :: _ ->
            case ( getRing ringStr, getQuadrant quadrantStr, getNew isNewStr ) of
                ( Ok ring, Ok quadrant, Ok isNew ) ->
                    Just <| Blip name rowNum ring quadrant isNew description Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


getRing : String -> Result String Ring
getRing ringStr =
    if ringStr == "hold" then
        Ok Hold
    else if ringStr == "assess" then
        Ok Assess
    else if ringStr == "trial" then
        Ok Trial
    else if ringStr == "adopt" then
        Ok Adopt
    else
        Err <| "Invalid ring value" ++ ringStr


getQuadrant : String -> Result String Quadrant
getQuadrant quadrantStr =
    if quadrantStr == "tools" then
        Ok Tools
    else if quadrantStr == "techniques" then
        Ok Techniques
    else if quadrantStr == "platforms" then
        Ok Platforms
    else if quadrantStr == "languages & frameworks" then
        Ok LangsAndFrameworks
    else
        Err <| "Invalid quadrant value" ++ quadrantStr


getNew : String -> Result String Bool
getNew isNewStr =
    if isNewStr == "TRUE" then
        Ok True
    else if isNewStr == "FALSE" then
        Ok False
    else
        Err <| "Invalid isNew value" ++ isNewStr



-- View


view : Model -> Html Msg
view model =
    div
        []
        [ urlInput model.url
        , showRadarButton
        , text <| Maybe.withDefault "" <| model.error_
        ]


urlInput : String -> Html Msg
urlInput url =
    input
        [ type_ "text"
        , value url
        , onInput UpdateUrl
        ]
        []


showRadarButton : Html Msg
showRadarButton =
    button
        [ onClick RetrieveRadarData ]
        [ text "Show radar" ]
