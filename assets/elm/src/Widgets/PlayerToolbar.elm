module Widgets.PlayerToolbar exposing (Config, view)

import Html exposing (Html, li, a, text, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

type alias Config msg =
  { joinLeaveMsg : msg
  , btnText : String
  , actionPressedMsg : msg
  }

view : Config msg -> Html msg
view config =
  div [ class "controls-container" ]
    [ li [ class "control-item" ]
      [ a [ onClick config.joinLeaveMsg ] [ text config.btnText ] ]
    , li [ class "control-item" ]
      [ a [ onClick config.actionPressedMsg ] [ text "Actions" ] ]
    , li [ class "control-item" ]
      [ a [ ] [ text "Account"] ]
    , li [ class "control-item" ]
      [ a [ ] [ text "Chat" ] ]
    , li [ class "control-item" ]
      [ a [ ] [ text "Bank" ] ]
    ]