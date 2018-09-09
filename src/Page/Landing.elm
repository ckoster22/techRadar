module Page.Landing exposing (Model, Msg(..), initialModel, update, view)

import Data.Radar exposing (GoogleSheetBlip, Quadrant(..), Ring(..))
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Maybe.Extra as MaybeExtra
import Regex exposing (Match, Regex, find, fromString)


type alias Model =
    { url : String
    , error_ : Maybe String
    , isLoading : Bool
    }


initialModel : Model
initialModel =
    Model "" Nothing False


type Msg
    = RetrieveRadarData
    | RetrieveRadarDataSuccess (List GoogleSheetBlip) (Maybe (List String))
    | RetrieveRadarDataFailure String
    | UpdateUrl String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RetrieveRadarData ->
            case findSheetId model.url of
                Ok sheetId ->
                    ( { model | isLoading = True }
                    , httpGetSheetById sheetId
                    )

                Err error ->
                    ( { model | error_ = Just error }
                    , Cmd.none
                    )

        RetrieveRadarDataSuccess _ _ ->
            -- This is handled in Main.elm
            ( model
            , Cmd.none
            )

        RetrieveRadarDataFailure error ->
            ( { model | error_ = Just error, isLoading = False }
            , Cmd.none
            )

        UpdateUrl url ->
            ( { model | url = url }
            , Cmd.none
            )


maybeSheetIdRegex : Maybe Regex
maybeSheetIdRegex =
    fromString "https:\\/\\/docs.google.com\\/spreadsheets\\/d\\/(.*?)($|\\/$|\\/.*|\\?.*)"


findSheetId : String -> Result String String
findSheetId url =
    case maybeSheetIdRegex of
        Nothing ->
            Err "internal error: illegal regex"

        Just sheetIdRegex ->
            find sheetIdRegex url
                |> List.head
                |> Maybe.map .submatches
                |> Maybe.map (List.head >> MaybeExtra.join)
                |> MaybeExtra.join
                |> Maybe.map Ok
                |> Maybe.withDefault (Err "Unable to parse Google Sheet ID")


httpResultToMsg : Result Http.Error String -> Msg
httpResultToMsg result =
    case result of
        Ok csv ->
            let
                sheetRows =
                    case String.split "\u{000D}\n" csv of
                        title :: rest ->
                            rest

                        _ ->
                            []

                ( blips, errors ) =
                    List.foldl
                        (\row ( blips1, errors1, index1 ) ->
                            case csvToBlipResult row index1 of
                                Ok blip ->
                                    ( blip :: blips1, errors1, index1 + 1 )

                                Err error ->
                                    ( blips1, error :: errors1, index1 + 1 )
                        )
                        ( [], [], 1 )
                        sheetRows
                        |> (\( blips1, errors1, _ ) ->
                                if List.length errors1 == 0 then
                                    ( blips1, Nothing )

                                else
                                    ( blips1, Just errors1 )
                           )
            in
            RetrieveRadarDataSuccess blips errors

        Err httpError ->
            RetrieveRadarDataFailure "Unable to retrieve Google Sheet"


httpGetSheetById : String -> Cmd Msg
httpGetSheetById sheetId =
    sheetJsonUrl sheetId
        |> Http.getString
        |> Http.send httpResultToMsg


sheetJsonUrl : String -> String
sheetJsonUrl sheetId =
    "https://docs.google.com/spreadsheets/d/" ++ sheetId ++ "/export?gid=0&format=csv"


csvToBlipResult : String -> Int -> Result String GoogleSheetBlip
csvToBlipResult csv rowNum =
    case String.split "," csv of
        name :: ringStr :: quadrantStr :: isNewStr :: description :: _ ->
            case ( getRing ringStr, getQuadrant quadrantStr, getNew isNewStr ) of
                ( Ok ring, Ok quadrant, Ok isNew ) ->
                    Ok <| GoogleSheetBlip name rowNum ring quadrant isNew description

                _ ->
                    Err <| "Row found with at least one unexpected value: " ++ csv

        _ ->
            Err <| "Row is not in the correct format: " ++ csv


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
        [ class "inputContainer" ]
        [ urlInput model.url
        , showRadarButton model.isLoading
        , text <| Maybe.withDefault "" <| model.error_
        ]


urlInput : String -> Html Msg
urlInput url =
    input
        [ type_ "text"
        , class "sheetInput"
        , value url
        , onInput UpdateUrl
        , placeholder "e.g. https://docs.google.com/spreadsheets/d/1waDG0_W3-yNiAaUfxcZhTKvl7AUCgXwQw8mdPjCz86U/"
        ]
        []


showRadarButton : Bool -> Html Msg
showRadarButton isLoading =
    button
        [ onClick RetrieveRadarData
        , class "sheetInputButton"
        ]
        [ text <| buttonText isLoading ]


buttonText : Bool -> String
buttonText isLoading =
    if isLoading then
        "Please wait.."

    else
        "Show radar"
