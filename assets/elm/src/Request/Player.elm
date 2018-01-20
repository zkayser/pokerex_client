module Request.Player exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.Player as Player exposing (Player, Username, encodeUsername)
import Data.PasswordReset as PasswordReset exposing (PasswordReset)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra
import Ports

apiUrl : String
apiUrl =
  "http://localhost:8080/api" -- Should be configurable, but hitting Phx server on this port for early development

storeSession : Player -> Cmd msg
storeSession player =
  Player.encode player
    |> Encode.encode 0
    |> Just
    |> Ports.storeSession

type alias Registration r =
  { r |
    username : String
  , password : String
  , email : String
  , firstName : String
  , lastName : String
  , blurb : String
  }

{-
Not sure if the detailed implementation of this is going to work right off the bat.
Might need to tweak the server side implementation, add routes for api session requests,
etc.; Also, this specific request should accept a json field with a "player" {"player": {...embeddedFields}}
-}
login : { r | username : String, password : String } -> Http.Request Player
login { username, password } =
  let
    player =
      Encode.object
        [ ( "username", Encode.string username )
        , ( "password", Encode.string password )
        ]

    body =
      Encode.object  [ ("player", player) ]
        |> Http.jsonBody

  in
  Decode.field "player" Player.decoder
    |> Http.post (apiUrl ++ "/sessions") body

facebookLogin : Encode.Value -> Http.Request Player
facebookLogin playerData =
  Decode.field "player" Player.decoder
    |> Http.post (apiUrl ++ "/auth") (playerData |> Http.jsonBody)

passwordReset : { r | email : String } -> Http.Request PasswordReset
passwordReset data =
  let
    body =
      Encode.object [ ("email", Encode.string data.email ) ]
        |> Http.jsonBody
  in
  Decode.field "data" PasswordReset.decoder
    |> Http.post (apiUrl ++ "/forgot_password") body

register : Registration r -> Http.Request Player
register registration =
  let
    registrationAttrs =
      Encode.object
        [ ("name", Encode.string registration.username)
        , ("email", Encode.string registration.email)
        , ("first_name", Encode.string registration.firstName)
        , ("last_name", Encode.string registration.lastName)
        , ("blurb", Encode.string registration.blurb)
        , ("password", Encode.string registration.password)
        ]

    body =
      Encode.object [ ("registration", registrationAttrs) ]
        |> Http.jsonBody
  in
  Decode.field "player" Player.decoder
    |> Http.post (apiUrl ++ "/registrations") body
