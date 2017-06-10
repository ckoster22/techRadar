module Landing.Util exposing (findSheetId, httpGetSheetById)

import Http
import Maybe.Extra as MaybeExtra
import Radar.Model exposing (csvToMaybeBlip)
import Regex exposing (HowMany(..), Match, Regex, find, regex)
import Types exposing (Msg(..))


sheetIdRegex : Regex
sheetIdRegex =
    regex "https:\\/\\/docs.google.com\\/spreadsheets\\/d\\/(.*?)($|\\/$|\\/.*|\\?.*)"


findSheetId : String -> Maybe String
findSheetId url =
    find All sheetIdRegex url
        |> List.head
        |> Maybe.map .submatches
        |> Maybe.map (List.head >> MaybeExtra.join)
        |> Maybe.withDefault Nothing


sheetJsonUrl : String -> String
sheetJsonUrl sheetId =
    "http://docs.google.com/spreadsheets/d/" ++ sheetId ++ "/export?gid=0&format=csv"


httpResultToMsg : Result Http.Error String -> Msg
httpResultToMsg result =
    case result of
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
            RetrieveRadarDataFailure "something bad happened"


httpGetSheetById : String -> Cmd Msg
httpGetSheetById sheetId =
    sheetJsonUrl sheetId
        |> Http.getString
        |> Http.send httpResultToMsg
