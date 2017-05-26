module Main exposing (main)


main : Program Never Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = Msg
