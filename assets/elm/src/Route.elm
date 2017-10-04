module Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing (Parser, parseHash, s, oneOf)

type Route
  = Home

route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "") ]

fromLocation : Location -> Maybe Route
fromLocation location =
  if String.isEmpty location.hash then
    Just Home
  else
    parseHash route location
