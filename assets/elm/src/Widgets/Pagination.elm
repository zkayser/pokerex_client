module Widgets.Pagination exposing (paginate, Config)

import Html as Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

type alias Model r =
  { r |
    totalPages : Int
  , page : Int
  }

type alias Config msg =
  { onClickMsg : String -> msg }

paginate : Model r -> Config msg -> Html msg
paginate model config =
  ul [ class "pagination pagination-list"]
    (List.map (\text -> viewPaginationItem model config text) (paginationText model.page model.totalPages))

viewPaginationItem : Model r -> Config msg -> String -> Html msg
viewPaginationItem model config paginationText =
  let
    active =
      case String.toInt paginationText of
        Ok num -> if model.page == num then "active" else ""
        Err _ -> ""
    disabledStart =
      if model.page == 1 && paginationText == "keyboard_arrow_left" then "disabled-page-icon" else ""
    disabledEnd =
      if model.page == model.totalPages && paginationText == "keyboard_arrow_right" then
        "disabled-page-icon"
      else
        ""
    extraClasses =
      String.join " " [active, disabledStart, disabledEnd]
  in
  li [ class (String.trim extraClasses) ]
    [ a [ onClick (config.onClickMsg paginationText) ] (viewItemText paginationText) ]

viewItemText : String -> List (Html msg)
viewItemText paginationText =
  if List.member paginationText ["keyboard_arrow_left", "keyboard_arrow_right"] then
    [ i [ class "material-icons" ] [ text paginationText ] ]
  else
    [ text paginationText ]

paginationText : Int -> Int -> List String
paginationText currentPage pageCount =
  let
    currentInterval =
      getPaginationIntervalFor currentPage
  in
  [ "keyboard_arrow_left" ]
  ++ (List.map (\num -> toString num) currentInterval)
  ++ [ "keyboard_arrow_right" ]

getPaginationIntervalFor : Int -> List Int
getPaginationIntervalFor currentPage =
  case currentPage <= 5 of
    True -> List.range 1 5
    False -> case currentPage % 5 == 0 of
      True -> List.range (currentPage - 4) currentPage
      False -> case currentPage % 5 == 1 of
        True -> List.range currentPage (currentPage + 4)
        False ->
          let
            distanceToStart =
              (currentPage % 5) - 1
            distanceToEnd =
              (5 - (currentPage % 5))
          in
          List.range (currentPage - distanceToStart) (currentPage + distanceToEnd)