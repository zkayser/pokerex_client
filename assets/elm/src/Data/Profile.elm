module Data.Profile exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode

type alias Profile =
  { errors : List Error
  , id : Int
  , username : String
  , email : String
  , chips : Int
  , blurb : String
  , isNewProfile : Bool
  }

type alias Error =
  ( Field, String )

type Field
  = Chips
  | E_mail
  | Blurb
  | Server

initialProfile : Profile
initialProfile =
  { errors = []
  , id = 0
  , username = ""
  , email = ""
  , chips = 1000
  , blurb = ""
  , isNewProfile = True
  }

decoder : Decoder Profile
decoder =
  decode Profile
    |> hardcoded [] -- Come back to this later
    |> required "id" Decode.int
    |> required "name" Decode.string
    |> required "email" Decode.string
    |> required "chips" Decode.int
    |> required "blurb" Decode.string
    |> hardcoded False