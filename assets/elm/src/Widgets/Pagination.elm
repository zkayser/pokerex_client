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
  { onClickMsg : String -> msg
  , linksToShow : Int
  }

paginate : Model r -> Config msg -> Html msg
paginate model config =
  let
    paginationHtml =
      case model.totalPages of
        1 -> text ""
        _ ->
          ul [ class "pagination pagination-list"]
            (List.map (\text -> viewPaginationItem model config text) (paginationText model config))
  in
  paginationHtml

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

paginationText : Model r -> Config msg -> List String
paginationText model config =
  let
    currentInterval =
      getPaginationIntervalFor model.page model.totalPages config.linksToShow
  in
  [ "keyboard_arrow_left" ]
  ++ (List.map (\num -> toString num) currentInterval)
  ++ [ "keyboard_arrow_right" ]

getPaginationIntervalFor : Int -> Int -> Int -> List Int
getPaginationIntervalFor currentPage totalPages numLinksToShow =
  case (totalPages <= numLinksToShow, totalPages <= currentPage, (currentPage + numLinksToShow) >= totalPages) of
    (True, _, _) -> List.range 1 totalPages
    (_, True, _) -> List.range (currentPage - numLinksToShow) currentPage
    (_, _, True) -> List.range (totalPages - numLinksToShow) totalPages
    _ -> List.range currentPage (currentPage + numLinksToShow)