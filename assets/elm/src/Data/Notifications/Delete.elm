module Data.Notifications.Delete exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Delete =
    { title : String
    , owner : String
    }


decoder : Decoder Delete
decoder =
    decode Delete
        |> required "title" Decode.string
        |> required "owner" Decode.string
