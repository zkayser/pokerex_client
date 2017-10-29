module Widgets.Modal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Data.Player as Player exposing (Player)

type alias Config msg =
  { backgroundColor : String
  , contentHtml : List (Html msg)  
  }

view : Config msg -> Html msg
view config =
  div [ class "modal-backdrop" ]
    [ div [ class ("modal-content card-panel " ++ config.backgroundColor) ]   
      config.contentHtml
    ]