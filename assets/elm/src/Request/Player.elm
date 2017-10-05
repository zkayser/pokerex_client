module Request.Player exposing (login, register, storeSession)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.Player as Player exposing (Player, Username, encodeUsername)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra
import Ports

apiUrl : String
apiUrl =
  "http://localhost:3000/api" -- Should be configurable, but hitting Phx server on this port for early development

storeSession : Player -> Cmd msg
storeSession player =
  Player.encode player
    |> Encode.encode 0
    |> Just
    |> Ports.storeSession

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

register : { r | username : String, email : String, password : String } -> Http.Request Player
register { username, email, password } =
  let
    player =
      Encode.object
        [ ("username", Encode.string username)
        , ("email", Encode.string email)
        , ("password", Encode.string password)
        ]

    body =
      Encode.object [ ("player", player) ]
        |> Http.jsonBody
  in
  Decode.field "player" Player.decoder
    |> Http.post (apiUrl ++ "/players") body
