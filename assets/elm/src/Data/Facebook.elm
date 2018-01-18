module Data.Facebook exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, decode)
import Json.Encode as Encode

type alias FBData = { name : String, id : String }

decoder : Decoder FBData
decoder =
  decode FBData
    |> required "name" Decode.string
    |> required "id" Decode.string

encode : FBData -> Encode.Value
encode data =
  Encode.object [("name", Encode.string data.name), ("facebook_id", Encode.string data.id)]