module Request.Player exposing (..)

import Data.AuthToken as AuthToken exposing (AuthToken, withAuthorization)
import Data.PasswordReset as PasswordReset exposing (PasswordReset)
import Data.Player as Player exposing (Player, Username, encodeUsername)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra
import Ports


storeSession : Player -> Cmd msg
storeSession player =
    Player.encode player
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession


type alias Registration r =
    { r
        | username : String
        , password : String
        , email : String
        , firstName : String
        , lastName : String
        , blurb : String
        , apiUrl : String
    }



-- TODO: Need to come back and reconsider
-- the naming of PasswordReset and ResetPassword.
-- The type below is the type that needs to be
-- sent to the server to actually execute the password reset.
-- `PasswordReset` right now is the payload used when a player
-- is on the `ForgotPassword` page and we want to tell the server
-- to initiate the process to reset the player's password.


type alias ResetPassword r =
    { r
        | resetToken : String
        , newPassword : String
        , apiUrl : String
    }



{-
   Not sure if the detailed implementation of this is going to work right off the bat.
   Might need to tweak the server side implementation, add routes for api session requests,
   etc.; Also, this specific request should accept a json field with a "player" {"player": {...embeddedFields}}
-}


login : { r | username : String, password : String, apiUrl : String } -> Http.Request Player
login { username, password, apiUrl } =
    let
        player =
            Encode.object
                [ ( "username", Encode.string username )
                , ( "password", Encode.string password )
                ]

        body =
            Encode.object [ ( "player", player ) ]
                |> Http.jsonBody
    in
    Decode.field "player" Player.decoder
        |> Http.post (apiUrl ++ "/sessions") body


facebookLogin : Encode.Value -> String -> Http.Request Player
facebookLogin playerData apiUrl =
    Decode.field "player" Player.decoder
        |> Http.post (apiUrl ++ "/auth") (playerData |> Http.jsonBody)


passwordReset : { r | email : String, apiUrl : String } -> Http.Request PasswordReset
passwordReset data =
    let
        apiUrl =
            data.apiUrl

        body =
            Encode.object [ ( "email", Encode.string data.email ) ]
                |> Http.jsonBody
    in
    Decode.field "data" PasswordReset.decoder
        |> Http.post (apiUrl ++ "/forgot_password") body


resetPassword : ResetPassword r -> Http.Request Player
resetPassword data =
    let
        apiUrl =
            data.apiUrl

        body =
            Encode.object
                [ ( "password", Encode.string data.newPassword )
                , ( "reset_token", Encode.string data.resetToken )
                ]
                |> Http.jsonBody
    in
    Decode.field "player" Player.decoder
        |> Http.post (apiUrl ++ "/reset_password") body


register : Registration r -> Http.Request Player
register registration =
    let
        apiUrl =
            registration.apiUrl

        registrationAttrs =
            Encode.object
                [ ( "name", Encode.string registration.username )
                , ( "email", Encode.string registration.email )
                , ( "first_name", Encode.string registration.firstName )
                , ( "last_name", Encode.string registration.lastName )
                , ( "blurb", Encode.string registration.blurb )
                , ( "password", Encode.string registration.password )
                ]

        body =
            Encode.object [ ( "registration", registrationAttrs ) ]
                |> Http.jsonBody
    in
    Decode.field "player" Player.decoder
        |> Http.post (apiUrl ++ "/registrations") body
