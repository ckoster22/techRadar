module Landing.Util exposing (findSheetId)

import Maybe.Extra as MaybeExtra
import Regex exposing (HowMany(..), Match, Regex, find, regex)


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
