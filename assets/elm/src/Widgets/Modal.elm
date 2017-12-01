module Widgets.Modal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Data.Player as Player exposing (Player)

type alias Config msg =
  { classes : List String
  , contentHtml : List (Html msg)
  , styles : Maybe (List (String, String))
  }

view : Config msg -> Html msg
view config =
  let
    classes =
      List.map (\str -> str ++ " ") config.classes 
      |> String.concat
      |> (++) "modal-content card-panel "
      |> String.trim
  in    
  div [ class "modal-backdrop" ]
    [ div [ class classes, styles config ]   
      config.contentHtml
    ]
    
bottomModalView : Config msg -> Html msg
bottomModalView config =
  let
    classes =
      List.map (\str -> str ++ " ") config.classes
      |> String.concat
      |> (++) "bottom-modal "
      |> String.trim
  in   
  div [ class "modal-backdrop" ]
    [ div [ class classes, styles config ] 
      config.contentHtml
    ]

styles : Config msg -> Html.Attribute msg
styles config =
  case config.styles of
    Nothing -> style []
    Just theStyles -> style theStyles