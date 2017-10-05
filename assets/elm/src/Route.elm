module Route exposing (..)

import Navigation exposing (Location)
import Html exposing (Attribute)
import Html.Attributes as Attr
import UrlParser as Url exposing (Parser, parsePath, s, oneOf)

type Route
  = Home
  | Login
  | Logout
  | Register

route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Register (s "register")
        ]

fromLocation : Location -> Maybe Route
fromLocation location =
  if location.pathname == "" then
    Just Home
  else
    parsePath route location

href : Route -> Attribute msg
href route =
  Attr.href (routeToString route)

modifyUrl : Route -> Cmd msg
modifyUrl =
  routeToString >> Navigation.modifyUrl

routeToString : Route -> String
routeToString page =
  case page of
    Home -> "/"
    Login -> "login"
    Logout -> "logout"
    Register -> "register"
