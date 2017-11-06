module Widgets.PlayerToolbar exposing (Config, view)

import Html exposing (Html, li, a, text, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

type alias Config msg =
  { joinLeaveMsg : msg
  , btnText : String
  }

view : Config msg -> Html msg
view config =
  div [ class "controls-container" ]
    [ li [ class "control-item" ]
      [ a [ onClick config.joinLeaveMsg ] [ text config.btnText ] ]
    , li [ class "control-item" ] 
      [ a [ ] [ text "Some other button" ] ]
    ]