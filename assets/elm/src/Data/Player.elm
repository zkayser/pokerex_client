module Data.Player exposing (..)

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
  , chips : Int
  }
  
type alias TablePlayer =
  { name : Username
  , chips : Int
  }

-- Serialization --
decoder : Decoder Player
decoder =
  decode Player
    |> required "email" Decode.string
    |> required "token" AuthToken.decoder
    |> required "username" usernameDecoder
    |> required "chips" Decode.int

encode : Player -> Value
encode player =
  Encode.object
    [ ("email", Encode.string player.email)
    , ("token", AuthToken.encode player.token)
    , ("username", encodeUsername player.username)
    , ("chips", Encode.int player.chips)
    ]
    
tablePlayerDecoder : Decoder TablePlayer
tablePlayerDecoder =
  decode TablePlayer
    |> required "name" usernameDecoder
    |> required "chips" Decode.int

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
