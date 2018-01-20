module Widgets.FacebookLogin exposing (viewFBLogin)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

viewFBLogin : msg -> Html msg
viewFBLogin msg =
  button [ class "btn auth-btn waves-effect blue darken-4 white-text", onClick msg ]
    [ text "Login with Facebook" ]