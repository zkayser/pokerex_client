module Route exposing (..)

import Navigation exposing (Location)
import Html exposing (Attribute)
import Html.Attributes as Attr
import UrlParser as Url exposing (Parser, parseHash, s, oneOf, (</>))

type Route
  = Home
  | Login
  | Logout
  | Register
  | Room String String
  | Rooms
  | Profile String

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
        Home -> []
        Login -> [ "login" ]
        Logout -> [ "logout" ]
        Register -> [ "register" ]
        Room roomType roomTitle -> [ "rooms", roomType, roomTitle ]
        Rooms -> [ "rooms" ]
        Profile user -> [ "profile", user ]
  in
  "#/" ++ String.join "/" pieces