module Data.Radar exposing (Blip, Position, Quadrant(..), Ring(..))


type alias Blip =
    { name : String
    , rowNum : Int
    , ring : Ring
    , quadrant : Quadrant
    , isNew : Bool
    , description : String
    , position_ : Maybe Position
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


type alias Position =
    { x : Float, y : Float }
