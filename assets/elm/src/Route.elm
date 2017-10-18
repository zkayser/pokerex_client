module Route exposing (..)

import Navigation exposing (Location)
import Html exposing (Attribute)
import Html.Attributes as Attr
import UrlParser as Url exposing (Parser, parseHash, s, oneOf)

-- TODO - Add more robust routing to Room routes
type Route
  = Home
  | Login
  | Logout
  | Register
  | Room -- This will need to be parameterized later on with a slug for the room.

route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Register (s "register")
        , Url.map Room (s "room")
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
  case page of
    Home -> "#/"
    Login -> "#/login"
    Logout -> "#/logout"
    Register -> "#/register"
    Room -> "#/room"
