module Radar.Model exposing (Blip, Quadrant, Radar, Ring)


type alias Radar =
    List Blip


type alias Blip =
    { name : String
    , ring : Ring
    , quadrant : Quadrant
    , isNew : Bool
    , description : String
    }


type Ring
    = Hold
    | Assess
    | Trial
    | Adopt


type Quadrant
    = Tools
    | Techniques
    | Platforms
    | LangsAndFrameworks
