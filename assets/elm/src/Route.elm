module Route exposing (..)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s)


type Route
    = Home
    | Login
    | Logout
    | Register
    | Room String String
    | Rooms
    | Profile String
    | ForgotPassword
    | ResetPassword String


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Register (s "register")
        , Url.map Room (s "rooms" </> Url.string </> Url.string)
        , Url.map Rooms (s "rooms")
        , Url.map Profile (s "profile" </> Url.string)
        , Url.map ForgotPassword (s "forgot-password")
        , Url.map ResetPassword (s "reset-password" </> Url.string)
        ]


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                Room roomType roomTitle ->
                    [ "rooms", roomType, roomTitle ]

                Rooms ->
                    [ "rooms" ]

                Profile user ->
                    [ "profile", user ]

                ForgotPassword ->
                    [ "forgot-password" ]

                ResetPassword resetToken ->
                    [ "reset-password", resetToken ]
    in
    "#/" ++ String.join "/" pieces
