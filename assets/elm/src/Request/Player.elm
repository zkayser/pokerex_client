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



type alias ResetPassword r =
    { r
        | resetToken : String
        , newPassword : String
        , apiUrl : String
    }


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
