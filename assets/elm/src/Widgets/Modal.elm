module Widgets.Modal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Data.Player as Player exposing (Player)

type alias Config msg =
  { backgroundColor : String
  , contentHtml : List (Html msg)
  , styles : Maybe (List (String, String))
  }

view : Config msg -> Html msg
view config =
  div [ class "modal-backdrop" ]
    [ div [ class ("modal-content card-panel " ++ config.backgroundColor), styles config ]   
      config.contentHtml
    ]
    
bottomModalView : Config msg -> Html msg
bottomModalView config =
  div [ class "modal-backdrop" ]
    [ div [ class ("bottom-modal " ++ config.backgroundColor), styles config ] 
      config.contentHtml
    ]

styles : Config msg -> Html.Attribute msg
styles config =
  case config.styles of
    Nothing -> style []
    Just theStyles -> style theStyles