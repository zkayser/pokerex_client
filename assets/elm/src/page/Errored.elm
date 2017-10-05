module Page.Errored exposing (..)

import Data.Session as Session exposing (Session)
import Html exposing (Html, div, h1, main_, p, text)
import Html.Attributes exposing (class, id)
import Views.Page as Page exposing (ActivePage)

type PageLoadError
  = PageLoadError Model

type alias Model =
  { activePage :  ActivePage
  , errorMessage : String
  }

pageLoadError : ActivePage -> String -> PageLoadError
pageLoadError activePage errorMessage =
  PageLoadError { activePage = activePage, errorMessage = errorMessage }

view : Session -> PageLoadError -> Html msg
view session (PageLoadError model) =
  main_ [ id "content", class "container" ]
    [ h1 [] [ text "Error Loading Page"]
    , div [ class "row" ]
      [ p [] [ text model.errorMessage ] ]
    ]
