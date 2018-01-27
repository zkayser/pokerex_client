module Data.Notifications.Invite exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, decode)

type alias Invite =
  { title : String
  , owner : String
  }

decoder : Decoder Invite
decoder =
  decode Invite
    |> required "title" Decode.string
    |> required "owner" Decode.string