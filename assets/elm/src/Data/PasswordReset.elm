module Data.PasswordReset exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, decode)

type alias PasswordReset =
  { message : String, type_ : RequestResult }

type RequestResult = Success | Error

decoder : Decoder PasswordReset
decoder =
  decode PasswordReset
    |> required "message" Decode.string
    |> required "type" resultDecoder

resultDecoder : Decoder RequestResult
resultDecoder =
  Decode.string
    |> Decode.andThen (\str -> Decode.succeed (stringToResult str))

stringToResult : String -> RequestResult
stringToResult string =
  case string of
    "success" -> Success
    _ -> Error