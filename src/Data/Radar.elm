module Data.Radar exposing (GoogleSheetBlip, Quadrant(..), Ring(..))


type alias GoogleSheetBlip =
    { name : String
    , rowNum : Int
    , ring : Ring
    , quadrant : Quadrant
    , isNew : Bool
    , description : String
    }


type Quadrant
    = Tools
    | Techniques
    | Platforms
    | LangsAndFrameworks


type Ring
    = Hold
    | Assess
    | Trial
    | Adopt
