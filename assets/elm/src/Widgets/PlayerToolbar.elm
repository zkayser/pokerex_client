module Widgets.PlayerToolbar exposing (Config, view, viewMobile)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

type alias Config msg =
  { joinLeaveMsg : msg
  , btnText : String
  , actionPressedMsg : msg
  , isActive : Bool
  , bankPressedMsg : msg
  , accountPressedMsg : msg
  , chatPressedMsg : msg
  }

view : Config msg -> Html msg
view config =
  div [ class "controls-container" ]
    [ li [ class "control-item" ]
      [ a [ onClick config.joinLeaveMsg ] [ text config.btnText ] ]
    , li [ class "control-item" ]
      [ viewActionBtn config ]
    , li [ class "control-item" ]
      [ a [ onClick config.accountPressedMsg ] [ text "Account"] ]
    , li [ class "control-item" ]
      [ a [ onClick config.chatPressedMsg ] [ text "Chat" ] ]
    , li [ class "control-item" ]
      [ a [ onClick config.bankPressedMsg ] [ text "Bank" ] ]
    ]
    
viewActionBtn : Config msg -> Html msg
viewActionBtn config =
  if config.isActive then
    a [ onClick config.actionPressedMsg, class "control-active" ] [ text "Actions" ]
  else
    a [] [ text "" ]

viewMobile : Config msg -> Html msg
viewMobile config =
  div [ class "mobile-controls-container btn-floating btn-large white-text red waves-effect" ]
    [ i [ class "material-icons"] [ text "add"] ]