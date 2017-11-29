module Data.Chat exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)

type alias Chat = 
  { playerName : String
  , message : String
  }

decoder : Decoder Chat
decoder =
  decode Chat
    |> required "player" Decode.string
    |> required "message" Decode.string