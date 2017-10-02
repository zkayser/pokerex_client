module Data.Player exposing ( Player
                            , Username
                            , decoder
                            , encode
                            , encodeUsername
                            , usernameDecoder
                            , usernameToHtml
                            , usernameToString)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import UrlParser

type alias Player =
  { email : String
  , token : AuthToken
  , username : Username
  }

-- Serialization --
decoder : Decoder Player
decoder =
  decode Player
    |> required "email" Decode.string
    |> required "token" AuthToken.decoder
    |> required "username" usernameDecoder

encode : Player -> Value
encode player =
  Encode.object
    [ ("email", Encode.string player.email)
    , ("token", AuthToken.encode player.token)
    , ("username", encodeUsername player.username)
    ]

-- Identifiers --

type Username
  = Username String

usernameToString : Username -> String
usernameToString (Username username) =
  username

usernameDecoder : Decoder Username
usernameDecoder =
  Decode.map Username Decode.string

encodeUsername : Username -> Value
encodeUsername (Username username) =
  Encode.string username

usernameToHtml : Username -> Html msg
usernameToHtml (Username username) =
  Html.text username
